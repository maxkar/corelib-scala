package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn

/**
 * Reader for (fixed) literals.
 * @param expected expected string.
 * @param errors error handler.
 */
final class LiteralReader[M[_]: Monad, S: LooksAheadIn[M]](
      expected: String,
      errors: LiteralReader.Errors[M, S]
    ) {

  /** Reads the literal from the stream. */
  def read(stream: S): M[Unit] =
    stream.fill(expected.length()) <+> check(stream, 0)


  /** Another name for read. */
  inline def apply(stream: S): M[Unit] = read(stream)


  /** Checks the string at the given offset. */
  private def check(stream: S, offset: Int): M[Unit] =
    stream.peek(offset) <||| { chr =>
      if chr != expected.charAt(offset) then
        errors.badLiteral(stream, expected, offset, chr)
      else {
        val nextOffset = offset + 1
        if nextOffset == expected.length then
          stream.skip(nextOffset)
        else
          check(stream, nextOffset)
      }
    }
}


/** Reader for JSON literals. */
object LiteralReader {
  /** Representation of the `true` literal. */
  val TRUE = "true"

  /** Representation of the `false` literal. */
  val FALSE = "false"

  /** Representation of the `null` literal. */
  val NULL = "null"

  /** Error handler for literal parsing errors. */
  trait Errors[M[_], S] {
    /**
     * Indicates the reader about invalid literal.
     * @param expected expected literal.
     * @param mismatchOffset offset where an actual character does not match the
     *   expected character.
     * @param actualChar actual character to read.
     */
    def badLiteral[T](stream: S, expected: String, mismatchOffset: Int, actualChar: Int): M[T]
  }
  type ErrorsIn[M[_]] = [S] =>> Errors[M, S]


  object Errors {
    def raise[M[_], S](raiseFn: [T] => (S, String) => M[T]): Errors[M, S] =
      new Errors[M, S] {
        override def badLiteral[T](stream: S, expected: String, mismatchOffset: Int, actualChar: Int): M[T] =
          raiseFn(stream, s"Invalid ${expected} literal")
      }
  }


  /** Readers for all literals. */
  case class Readers[M[_], S](
      trueLiteral: LiteralReader[M, S],
      falseLiteral: LiteralReader[M, S],
      nullLiteral: LiteralReader[M, S]
  )


  def trueReader[M[_]: Monad, S: LooksAheadIn[M]](using errors: Errors[M, S]): LiteralReader[M, S] =
    new LiteralReader(TRUE, errors)

  def falseReader[M[_]: Monad, S: LooksAheadIn[M]](using errors: Errors[M, S]): LiteralReader[M, S] =
    new LiteralReader(FALSE, errors)

  def nullReader[M[_]: Monad, S: LooksAheadIn[M]](using errors: Errors[M, S]): LiteralReader[M, S] =
    new LiteralReader(NULL, errors)

  def all[M[_]: Monad, S: LooksAheadIn[M]: ErrorsIn[M]](): Readers[M, S] =
    Readers(
      trueLiteral = trueReader,
      falseLiteral = falseReader,
      nullLiteral = nullReader
    )
}
