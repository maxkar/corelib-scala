package io.github.maxkar
package json.parser

import fun.typeclass.Functor
import fun.typeclass.Applicative
import fun.typeclass.Monad

/** Object parsers. */
object Objects {
  /**
   * Factory that creates a 'J' representation of a JSON object
   * from the stream `S`.
   */
  trait Factory[M[_], -S, J] {
    /** Context of an object being built. */
    type Context
    /** Type of the key. */
    type Key

    /**
     * Conusmes whitespaces that are not captured and do not affect reading
     * object values.
     */
    def skipIgnorableWhitespaces(stream: S): M[Unit]

    /**
     * Starts the object by consuming the object start character
     * and creating the ("empty") context.
     *
     * @param stream stream being parsed.
     * @param count number of characters in the object start. This number
     *   of characters should be consumed.
     */
    def start(stream: S, count: Int): M[Context]

    /** Handles a situation where object start was expected but was not found. */
    def invalidObjectStart(stream: S): M[J]

    /** Consumes key from the stream and returns its representation. */
    def readKey(stream: S, context: Context): M[Key]

    /** Consumes key-value separator from the stream. */
    def skipKeyValueSeparator(stream: S, context: Context, key: Key, count: Int): M[Unit]

    /** Handles a situation where key-value separator is missing. */
    def invalidKeyValueSeparator(stream: S, context: Context, key: Key): M[Unit]

    /** Cosumes value for the given key. */
    def readValue(stream: S, context: Context, key: Key): M[Unit]

    /** Consumes entry separator from the stream. */
    def skipEntrySeparator(stream: S, context: Context, count: Int): M[Unit]

    /** Finishes reading the object and returns its JSON representation. */
    def finish(stream: S, context: Context, count: Int): M[J]

    /**
     * Handles a situation where entry separator or object end was expected
     * but neither was found.
     */
    def invalidEntrySeparatorOrObjectEnd(stream: S, context: Context): M[J]
  }


  object Factory {
    abstract class RaiseParseErrors[M[_], -S: ParseError.In[M], J] extends Factory[M, S, J] {
      override final def invalidObjectStart(stream: S): M[J] =
        stream.parseError("Invalid object start")
      override final def invalidKeyValueSeparator(stream: S, context: Context, key: Key): M[Unit] =
        stream.parseError("Invalid key-value separator")
      override final def invalidEntrySeparatorOrObjectEnd(stream: S, context: Context): M[J] =
        stream.parseError("Invalid entry separator or object end")
    }


    abstract class Simple[M[_]: Functor, -S: SkipStream.In[M]: ParseError.In[M], J] extends RaiseParseErrors[M, S, J]{
      /** Creates a context. */
      def createContext(): Context

      /** Converts context to json value. */
      def createValue(context: Context): J

      override def skipIgnorableWhitespaces(stream: S): M[Unit] =
        Whitespaces.skip(stream)

      override final def start(stream: S, count: Int): M[Context] =
        stream.skip(count) >-| createContext()

      /** Consumes key-value separator from the stream. */
      override final def skipKeyValueSeparator(stream: S, context: Context, key: Key, count: Int): M[Unit] =
        stream.skip(count)

      override final def skipEntrySeparator(stream: S, context: Context, count: Int): M[Unit] =
        stream.skip(count)

      override final def finish(stream: S, context: Context, count: Int): M[J] =
        stream.skip(count) >-| createValue(context)
    }


    open class AsMap[M[_]: Monad, -S: SkipStream.In[M]: ParseError.In[M], K, V](
          readKey: S => M[K],
          readValue: S => M[V],
        ) extends Simple[M, S, Map[K, V]] {

      override type Key = K
      override type Context = scala.collection.mutable.HashMap[K, V]

      /** Merges two values of the same key on the same object together. */
      def merge(key: K, v1: V, v2: V): M[V] =
        Applicative.pure(v2)

      override def createContext(): Context =
        new Context()

      override def createValue(context: Context): Map[K, V] =
        context.toMap

      override def readKey(stream: S, context: Context): M[Key] =
        readKey(stream)

      override def readValue(stream: S, context: Context, key: Key): M[Unit] =
        context.get(key) match {
          case None => readValue(stream) >-> {(v: V) => context.put(key, v)}
          case Some(oldValue) =>
            readValue(stream) >=>>
              {(v: V) => merge(key, oldValue, v)} >->
              {(v: V) => context.put(key, v) }
        }
    }
  }


  /** Reads the object and creates its representation by using the provided factory. */
  def read[M[_]: Monad, S: Peek.In[M], J](factory: Factory[M, S, J])(stream: S): M[J] =
    stream.peek(0) >=>> { chr =>
      if chr != '{' then
        factory.invalidObjectStart(stream)
      else
        factory.start(stream, 1) >=>> { context =>
          factory.skipIgnorableWhitespaces(stream) >=||
          stream.peek(0) >=>> {
            case '}' => factory.finish(stream, context, 1)
            case _ => readValues(stream, factory, context)
          }
        }
    }


  /**
   * Reads values from the stream and "appends" them to the context.
   * This method could be used inside `missingValueSeparatorOrObjectEnd` to
   * recover from an error and continue array parsing.
   */
  def readValues[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context,
      ): M[J] =
    factory.skipIgnorableWhitespaces(stream) >=||
    (factory.readKey(stream, context) >=>> { key =>
      factory.skipIgnorableWhitespaces(stream) >=||
      stream.peek(0) >=>>{
        case ':' => factory.skipKeyValueSeparator(stream, context, key, 1)
        case _ => factory.invalidKeyValueSeparator(stream, context, key)
      } >=||
      factory.skipIgnorableWhitespaces(stream) >=||
      factory.readValue(stream, context, key) >=||
      factory.skipIgnorableWhitespaces(stream) >=||
      (stream.peek(0) >=>> {
        case '}' => factory.finish(stream, context, 1)
        case ',' => factory.skipEntrySeparator(stream, context, 1) >=|| readValues(stream, factory, context)
        case _ => factory.invalidEntrySeparatorOrObjectEnd(stream, context)
      })
    })
}
