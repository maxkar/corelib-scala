package io.github.maxkar
package json.parser

import fun.typeclass.Functor
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
  trait Factory[M[_], -S, J] {
    /**
     * Consumes `count` characters from the stream and returns the
     * JSON literal representation.
     */
    def read(stream: S, count: Int): M[J]

    /**
     * Processes an invalid (bad) literal
     * @param stream stream with the data.
     * @param expected expected literal.
     * @return JSON representation of the invalid literal.
     */
    def invalidLiteral(stream: S, expected: String): M[J]
  }


  object Factory {
    abstract class RaiseParseErrors[M[_], -S: ParseError.In[M], J] extends Factory[M, S, J] {
      override final def invalidLiteral(stream: S, expected: String): M[J] =
        stream.parseError(s"Invalid ${expected} literal")
    }

    /** Simple factory that consumes literal and returns the specified value for default stream. */
    final class Simple[M[_]: Functor, S: SkipStream.In[M]: ParseError.In[M], J](
          value: J
        ) extends Factory.RaiseParseErrors[M, S, J] {
      override def read(stream: S, count: Int): M[J] =
        stream.skip(count) >-| value
    }
  }


  /**
   * Consumes the "true" literal from the stream and returns the value
   * created by the factory.
   */
  def readNull[M[_]: Monad, S: Peek.In[M], J](factory: Factory[M, S, J])(stream: S): M[J] =
    for
      c1 <- stream.peek(0)
      c2 <- stream.peek(1)
      c3 <- stream.peek(2)
      c4 <- stream.peek(3)
      res <-
        if c1 != 'n' || c2 != 'u' || c3 != 'l' || c4 != 'l' then
          factory.invalidLiteral(stream, NULL)
        else
          factory.read(stream, 4)
    yield res


  /**
   * Consumes the "true" literal from the stream and returns the value
   * created by the factory.
   */
  def readTrue[M[_]: Monad, S: Peek.In[M], J](factory: Factory[M, S, J])(stream: S): M[J] =
    for
      c1 <- stream.peek(0)
      c2 <- stream.peek(1)
      c3 <- stream.peek(2)
      c4 <- stream.peek(3)
      res <-
        if c1 != 't' || c2 != 'r' || c3 != 'u' || c4 != 'e' then
          factory.invalidLiteral(stream, TRUE)
        else
          factory.read(stream, 4)
    yield res


  /**
   * Consumes the "false" literal from the stream and returns the value
   * created by the factory.
   */
  def readFalse[M[_]: Monad, S: Peek.In[M], J](factory: Factory[M, S, J])(stream: S): M[J] =
    for
      c1 <- stream.peek(0)
      c2 <- stream.peek(1)
      c3 <- stream.peek(2)
      c4 <- stream.peek(3)
      c5 <- stream.peek(4)
      res <-
        if c1 != 'f' || c2 != 'a' || c3 != 'l' || c4 != 's' || c5 != 'e' then
          factory.invalidLiteral(stream, FALSE)
        else
          factory.read(stream, 5)
    yield res
}
