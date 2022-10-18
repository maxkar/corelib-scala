package io.github.maxkar
package json.parser

import fun.typeclass.Monad
import fun.typeclass.Applicative

/**
 * Literal readers.
 */
object Literals:
  /**
   * Error handlers for (simple) JSON literals.
   * @tparam M execution (monad).
   * @tparam S type of the stream (i.e. "context") required by the error generation.
   */
  trait Errors[M[_], -S]:
    /**
     * Invoked when specific literal (text) was expected but stream content was different.
     * Stream position is before the first character of the literal in the stream.
     */
    def badLiteral(expected: String, stream: S): M[Unit]
  end Errors


  /** Representation of the `true` literal. */
  val TRUE = "true"

  /** Representation of the `false` literal. */
  val FALSE = "false"

  /** Representation of the `null` literal. */
  val NULL = "null"


  /**
   * Compares string (expected literal) and actual literal in the stream.
   * @param literal expected literal.
   * @param characters the input sequence (that should be not shorter than
   *   the literal).
   */
  def isSameLiteral(literal: String, characters: CharSequence): Boolean =
    if characters.length() < literal.length() then
      return false
    var ptr = 0
    while ptr < literal.length() do
      if literal.charAt(ptr) != characters.charAt(ptr) then
        return false
      ptr += 1
    return true
  end isSameLiteral


  /**
   * Checks if the input (or at least its prefix) is the same as the given literal.
   */
  def startsWithLiteral[M[_]: Applicative](
        literal: String,
        stream: CharacterStream[M])
      : M[Boolean] =
    stream.peek(literal.length()) map { isSameLiteral(literal, _) }


  /**
   * Reads the literal from the input stream.
   * @param literal expected literal to read.
   * @param stream stream that must contain the literal exactly at the defined position.
   */
  def readLiteral[M[_]: Monad, S <: CharacterStream[M]](
        literal: String,
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    stream.peek(literal.length()) flatMap { lookAhead =>
      if isSameLiteral(literal, lookAhead) then
        stream.skip(literal.length())
      else
        errs.badLiteral(literal, stream)
    }
  end readLiteral


  /** Reads the "true" literal from the stream. */
  inline def readTrue[M[_]: Monad, S <: CharacterStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    readLiteral(TRUE, stream)


  /** Reads the "false" literal from the stream. */
  inline def readFalse[M[_]: Monad, S <: CharacterStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    readLiteral(FALSE, stream)


  /** Reads the "null" literal from the stream. */
  inline def readNull[M[_]: Monad, S <: CharacterStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    readLiteral(NULL, stream)
end Literals
