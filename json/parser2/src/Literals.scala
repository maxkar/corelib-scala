package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Monad
import fun.Applicative

/**
 * Literal readers.
 */
object Literals:
  /** Handler for bad literals. */
  trait Errors[M[_]]:
    /**
     * The method is invoked when input does not match the expected literal.
     */
    def badLiteral(expected: String, actual: CharSequence): M[Unit]
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
  def readLiteral[M[_]: Monad: Errors](
        literal: String,
        stream: CharacterStream[M])
      : M[Unit] =
    stream.peek(literal.length()) flatMap { lookAhead =>
      if isSameLiteral(literal, lookAhead) then
        stream.skip(literal.length())
      else
        implicitly[Errors[M]].badLiteral(
          literal,
          lookAhead.subSequence(0, Math.min(literal.length(), lookAhead.length()))
        )
    }
  end readLiteral


  /** Reads the "true" literal from the stream. */
  inline def readTrue[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Unit] =
    readLiteral(TRUE, stream)


  /** Reads the "false" literal from the stream. */
  inline def readFalse[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Unit] =
    readLiteral(FALSE, stream)


  /** Reads the "null" literal from the stream. */
  inline def readNull[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Unit] =
    readLiteral(NULL, stream)
end Literals
