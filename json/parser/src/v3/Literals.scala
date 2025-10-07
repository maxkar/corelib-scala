package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad

object Literals {
  /** Representation of the `true` literal. */
  val TRUE = "true"

  /** Representation of the `false` literal. */
  val FALSE = "false"

  /** Representation of the `null` literal. */
  val NULL = "null"


  /**
   * Value factory that creates a JSON representation `J` of
   * a literal read from stream `S`.
   */
  trait Factory[M[_], S, J] {
    /**
     * Consumes `count` characters from the stream and returns the
     * JSON literal representation.
     */
    def consume(stream: S, count: Int): M[J]

    /**
     * Processes an invalid (bad) literal
     * @param stream stream with the data.
     * @param expected expected literal.
     * @param badOffset offset where stream does not match the literal.
     * @return JSON representation of the invalid literal.
     */
    def badLiteral(stream: S, expected: String, badOffset: Int): M[J]
  }


  /**
   * Consumes the "true" literal from the stream and returns the value
   * created by the factory.
   */
  def readNull[M[_]: Monad, S: Peek.In[M], J](stream: S, factory: Factory[M, S, J]): M[J] =
    for
      c1 <- stream.peek(0)
      c2 <- stream.peek(1)
      c3 <- stream.peek(2)
      c4 <- stream.peek(3)
      res <-
        if c1 != 'n' then factory.badLiteral(stream, NULL, 0)
        else if c2 != 'u' then factory.badLiteral(stream, NULL, 1)
        else if c3 != 'l' then factory.badLiteral(stream, NULL, 2)
        else if c4 != 'l' then factory.badLiteral(stream, NULL, 3)
        else factory.consume(stream, 4)
    yield res


  /**
   * Consumes the "true" literal from the stream and returns the value
   * created by the factory.
   */
  def readTrue[M[_]: Monad, S: Peek.In[M], J](stream: S, factory: Factory[M, S, J]): M[J] =
    for
      c1 <- stream.peek(0)
      c2 <- stream.peek(1)
      c3 <- stream.peek(2)
      c4 <- stream.peek(3)
      res <-
        if c1 != 't' then factory.badLiteral(stream, TRUE, 0)
        else if c2 != 'r' then factory.badLiteral(stream, TRUE, 1)
        else if c3 != 'u' then factory.badLiteral(stream, TRUE, 2)
        else if c4 != 'e' then factory.badLiteral(stream, TRUE, 3)
        else factory.consume(stream, 4)
    yield res


  /**
   * Consumes the "false" literal from the stream and returns the value
   * created by the factory.
   */
  def readFalse[M[_]: Monad, S: Peek.In[M], J](stream: S, factory: Factory[M, S, J]): M[J] =
    for
      c1 <- stream.peek(0)
      c2 <- stream.peek(1)
      c3 <- stream.peek(2)
      c4 <- stream.peek(3)
      c5 <- stream.peek(4)
      res <-
        if c1 != 'f' then factory.badLiteral(stream, FALSE, 0)
        else if c2 != 'a' then factory.badLiteral(stream, FALSE, 1)
        else if c3 != 'l' then factory.badLiteral(stream, FALSE, 2)
        else if c4 != 's' then factory.badLiteral(stream, FALSE, 3)
        else if c5 != 'e' then factory.badLiteral(stream, FALSE, 4)
        else factory.consume(stream, 5)
    yield res
}
