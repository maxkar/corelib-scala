package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad

/** Object parsers. */
object Objects {
  /**
   * Factory that creates a 'J' representation of a JSON object
   * from the stream `S`.
   */
  trait Factory[M[_], S, J] {
    /** Context of an object being built. */
    type Context
    /** Type of the key. */
    type Key

    /**
     * Conusmes whitespaces that are not captured and do not affect reading
     * object values.
     */
    def consumeIgnorableWhitespaces(stream: S): M[Unit]

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
    def badObjectStart(stream: S): M[J]

    /** Consumes key from the stream and returns its representation. */
    def consumeKey(stream: S, context: Context): M[Key]

    /** Consumes key-value separator from the stream. */
    def consumeKeyValueSeparator(stream: S, context: Context, key: Key, count: Int): M[Unit]

    /** Handles a situation where key-value separator is missing. */
    def missingKeyValueSeparator(stream: S, context: Context, key: Key): M[Unit]

    /** Cosumes value for the given key. */
    def consumeValue(stream: S, context: Context, key: Key): M[Unit]

    /** Consumes entry separator from the stream. */
    def consumeEntrySeparator(stream: S, context: Context, count: Int): M[Unit]

    /** Finishes reading the object and returns its JSON representation. */
    def finish(stream: S, context: Context, count: Int): M[J]

    /**
     * Handles a situation where entry separator or object end was expected
     * but neither was found.
     */
    def missingEntrySeparatorOrObjectEnd(stream: S, context: Context): M[J]
  }


  /** Reads the object and creates its representation by using the provided factory. */
  def read[M[_]: Monad, S: Peek.In[M], J](stream: S, factory: Factory[M, S, J]): M[J] =
    stream.peek(0) >=>> { chr =>
      if chr != '{' then
        factory.badObjectStart(stream)
      else
        factory.start(stream, 1) >=>> { context =>
          factory.consumeIgnorableWhitespaces(stream) >=||
          (stream.peek(0) >=>> {
            case '}' => factory.finish(stream, context, 1)
            case _ => readValues(stream, factory, context)
          })
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
    factory.consumeIgnorableWhitespaces(stream) >=||
    (factory.consumeKey(stream, context) >=>> { key =>
      factory.consumeIgnorableWhitespaces(stream) >=||
      (stream.peek(0) >=>>{
        case ':' => factory.consumeKeyValueSeparator(stream, context, key, 1)
        case _ => factory.missingKeyValueSeparator(stream, context, key)
      }) >=||
      factory.consumeIgnorableWhitespaces(stream) >=||
      factory.consumeValue(stream, context, key) >=||
      factory.consumeIgnorableWhitespaces(stream) >=||
      (stream.peek(0) >=>> {
        case '}' => factory.finish(stream, context, 1)
        case ',' => factory.consumeEntrySeparator(stream, context, 1) >=|| readValues(stream, factory, context)
        case _ => factory.missingEntrySeparatorOrObjectEnd(stream, context)
      })
    })
}
