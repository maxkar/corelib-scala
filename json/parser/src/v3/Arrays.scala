package io.github.maxkar
package json.parser.v3

import fun.typeclass.Functor
import fun.typeclass.Monad

object Arrays {
  /**
   * Factory for creating a `J` representation of a JSON
   * array read from the stream `S`.
   */
  trait Factory[M[_], -S, J] {
    /** Context of the array being built. */
    type Context

    /**
     * Conusmes whitespaces that are not captured and do not affect reading
     * array values.
     */
    def skipIgnorableWhitespaces(stream: S): M[Unit]

    /**
     * Starts the array by consuming the array start character
     * and creating the ("empty") context.
     *
     * @param stream stream being parsed.
     * @param count number of characters in the array start. This number
     *   of characters should be consumed.
     */
    def start(stream: S, count: Int): M[Context]

    /** Handles a situation where array start was expected but was not found. */
    def invalidArrayStart(stream: S): M[J]

    /** Consumes one value belonging to the array. */
    def readValue(stream: S, context: Context): M[Unit]

    /** Consumes value separator. */
    def skipValueSeparator(stream: S, context: Context, count: Int): M[Unit]

    /** Consumes end of the array and returns JSON array representation. */
    def finish(stream: S, context: Context, count: Int): M[J]

    /**
     * Handles a situation where value separator or array end was expected
     * but neither was found.
     */
    def invalidValueSeparatorOrArrayEnd(stream: S, context: Context): M[J]
  }


  object Factory {
    abstract class RaiseParseErrors[M[_], -S: ParseError.In[M], J] extends Factory[M, S, J] {
      /** Handles a situation where array start was expected but was not found. */
      override final def invalidArrayStart(stream: S): M[J] =
        stream.parseError("Invalid array start")
      override final def invalidValueSeparatorOrArrayEnd(stream: S, context: Context): M[J] =
        stream.parseError("Invalid value separator or array end")
    }


    abstract class Simple[M[_]: Functor, -S: DefaultStream.In[M]: ParseError.In[M], J] extends RaiseParseErrors[M, S, J] {
      /** Creates a context. */
      def createContext(): Context

      /** Converts context to json value. */
      def createValue(context: Context): J

      override def skipIgnorableWhitespaces(stream: S): M[Unit] = Whitespaces.skip(stream)

      /**
       * Starts the array by consuming the array start character
       * and creating the ("empty") context.
       *
       * @param stream stream being parsed.
       * @param count number of characters in the array start. This number
       *   of characters should be consumed.
       */
      override final def start(stream: S, count: Int): M[Context] =
        stream.skip(count) >-| createContext()

      /** Consumes value separator. */
      override final def skipValueSeparator(stream: S, context: Context, count: Int): M[Unit] =
        stream.skip(count)

      override final def finish(stream: S, context: Context, count: Int): M[J] =
        stream.skip(count) >-| createValue(context)
    }


    /** Reader that builds a sequence of elements. */
    final class AsSequence[M[_]: Functor, -S: DefaultStream.In[M]: ParseError.In[M], J](readValue: S => M[J])
          extends Simple[M, S, Seq[J]] {
      override type Context = scala.collection.mutable.ArrayBuffer[J]

      override def createContext(): Context =
        new Context()
      override def createValue(context: Context): Seq[J] =
        context.toSeq
      override def readValue(stream: S, context: Context): M[Unit] =
        readValue(stream) >-> (context.append)
    }
  }


  /** Reads the array and creates it representation using the given factory. */
  def read[M[_]: Monad, S: Peek.In[M], J](factory: Factory[M, S, J])(stream: S) : M[J] =
    stream.peek(0) >=>> { chr =>
      if chr != '[' then
        factory.invalidArrayStart(stream)
      else
        factory.start(stream, 1) >=>> { context =>
          factory.skipIgnorableWhitespaces(stream) >=||
          (stream.peek(0) >=>> {
            case ']' => factory.finish(stream, context, 1)
            case _ => readValues(stream, factory, context)
          })
        }
    }


  /**
   * Reads values from the stream and "appends" them to the context.
   * This method could be used inside `missingValueSeparatorOrArrayEnd` to
   * recover from an error and continue array parsing.
   */
  def readValues[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context,
      ): M[J] =
    factory.skipIgnorableWhitespaces(stream) >=||
    factory.readValue(stream, context) >=||
    factory.skipIgnorableWhitespaces(stream) >=||
    stream.peek(0) >=>> {
      case ']' => factory.finish(stream, context, 1)
      case ',' => factory.skipValueSeparator(stream, context, 1) >=|| readValues(stream, factory, context)
      case _ => factory.invalidValueSeparatorOrArrayEnd(stream, context)
    }
}
