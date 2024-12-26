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
final class LiteralReader[M[_]: Monad, T: LooksAheadIn[M]](
      expected: String,
      errors: LiteralReader.Errors[M]
    ) {

  /** Reads the literal from the stream. */
  def read(stream: T): M[Unit] =
    stream.fill(expected.length()) <+> check(stream, 0)

  /** Another name for read. */
  inline def apply(stream: T): M[Unit] = read(stream)


  /** Checks the string at the given offset. */
  private def check(stream: T, offset: Int): M[Unit] =
    stream.peek(offset) <||| { chr =>
      if chr != expected.charAt(offset) then
        errors.badLiteral(expected, offset, chr)
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
  trait Errors[M[_]] {
    /**
     * Indicates the reader about invalid literal.
     * @param expected expected literal.
     * @param mismatchOffset offset where an actual character does not match the
     *   expected character.
     * @param actualChar actual character to read.
     */
    def badLiteral[T](expected: String, mismatchOffset: Int, actualChar: Int): M[T]
  }


  /** Readers for all literals. */
  case class Readers[M[_], T](
      trueLiteral: LiteralReader[M, T],
      falseLiteral: LiteralReader[M, T],
      nullLiteral: LiteralReader[M, T]
  )


  def trueReader[M[_]: Monad, T: LooksAheadIn[M]](using errors: Errors[M]): LiteralReader[M, T] =
    new LiteralReader(TRUE, errors)

  def falseReader[M[_]: Monad, T: LooksAheadIn[M]](using errors: Errors[M]): LiteralReader[M, T] =
    new LiteralReader(FALSE, errors)

  def nullReader[M[_]: Monad, T: LooksAheadIn[M]](using errors: Errors[M]): LiteralReader[M, T] =
    new LiteralReader(NULL, errors)

  def all[M[_]: Monad: Errors, T: LooksAheadIn[M]](): Readers[M, T] =
    Readers(
      trueLiteral = trueReader,
      falseLiteral = falseReader,
      nullLiteral = nullReader
    )
}
