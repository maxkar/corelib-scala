package io.github.maxkar
package json.parser.v3

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
    def consumeIgnorableWhitespaces(stream: S): M[Unit]

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
    def badArrayStart(stream: S): M[J]

    /** Consumes one value belonging to the array. */
    def consumeValue(stream: S, context: Context): M[Unit]

    /** Consumes value separator. */
    def consumeValueSeparator(stream: S, context: Context, count: Int): M[Unit]

    /** Consumes end of the array and returns JSON array representation. */
    def finish(stream: S, context: Context, count: Int): M[J]

    /**
     * Handles a situation where value separator or array end was expected
     * but neither was found.
     */
    def missingValueSeparatorOrArrayEnd(stream: S, context: Context): M[J]
  }


  /** Reads the array and creates it representation using the given factory. */
  def read[M[_]: Monad, S: Peek.In[M], J](stream: S, factory: Factory[M, S, J]): M[J] =
    stream.peek(0) <||| { chr =>
      if chr != '[' then
        factory.badArrayStart(stream)
      else
        factory.start(stream, 1) <||| { context =>
          factory.consumeIgnorableWhitespaces(stream) <+>
          (stream.peek(0) <||| {
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
    factory.consumeIgnorableWhitespaces(stream) <+>
    factory.consumeValue(stream, context) <+>
    factory.consumeIgnorableWhitespaces(stream) <+>
    (stream.peek(0) <||| {
      case ']' => factory.finish(stream, context, 1)
      case ',' => factory.consumeValueSeparator(stream, context, 1) <+> readValues(stream, factory, context)
      case _ => factory.missingValueSeparatorOrArrayEnd(stream, context)
    })
}
