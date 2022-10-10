package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Monad
import scala.collection.mutable.ArrayBuffer


/** Array readers and related utilities. */
object Arrays {

  /** Error encoding for array formats. */
  trait Errors[M[_]]:
    /** Encodes invalid array start. */
    def invalidArrayStart[T](): M[T]

    /**
     * Encodes situation where array separator or array end character
     * is expected but no such element is found.
     */
    def arraySeparatorOrEndIsMissing[T](): M[T]
  end Errors

  /** Checks if character is an array start character. */
  def isArrayStart(char: Char): Boolean =
    char == '['


  /** Checks if character is indicator of array end character. */
  def isArrayEnd(char: Char): Boolean =
    char == ']'


  /** Checks if character is array element separator. */
  def isArraySeparator(char: Char): Boolean =
    char == ','


  /** Reads start of the array - the leading character. */
  def readArrayStart[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Unit] =
    stream.peek(0) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isArrayEnd(lookAhead.charAt(0)) then
        implicitly[Errors[M]].invalidArrayStart()
      else
        stream.skip(1)
    }


  /**
   * Checks if array is an empty array or not after the start character was read.
   * Consumes array end character if array is empty, otherwise leaves input intact.
   */
  def hasFirstValue[M[_]: Monad](stream: CharacterStream[M]): M[Boolean] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isArrayEnd(lookAhead.charAt(0)) then
        Monad.pure(true)
      else
        stream.skip(1) map { _ => false }
    }
  end hasFirstValue


  /**
   * Checks if array has next element or it is array end. Consumes the separator or end
   * character from input.
   */
  def hasNextValue[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Boolean] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 then
        implicitly[Errors[M]].arraySeparatorOrEndIsMissing()
      else
        lookAhead.charAt(0) match {
          case ',' => stream.skip(1) map { _ => true }
          case ']' => stream.skip(1) map { _ => false }
          case _ => implicitly[Errors[M]].arraySeparatorOrEndIsMissing()
        }
    }
  end hasNextValue


  /**
   * Reads complete array as a sequence.
   * Whitespace and value reading is delegated to provided handlers.
   */
  def readAll[T, M[_]: Monad: Errors](
        skipWhitespaces: CharacterStream[M] => M[Unit],
        readValue: CharacterStream[M] => M[T],
        stream: CharacterStream[M],
      ): M[Seq[T]] =
    for
      _ <- readArrayStart(stream)
      _ <- skipWhitespaces(stream)
      nonEmpty <- hasFirstValue(stream)
      res <-
        if nonEmpty then
          readAllImpl(skipWhitespaces, readValue, stream, new ArrayBuffer)
        else
          Monad.pure(Seq.empty)
    yield
      res
  end readAll


  /** Recursive reader implementation. */
  private def readAllImpl[T, M[_]: Monad: Errors](
        skipWhitespaces: CharacterStream[M] => M[Unit],
        readValue: CharacterStream[M] => M[T],
        stream: CharacterStream[M],
        agg: ArrayBuffer[T],
      ): M[Seq[T]] =
    for
      elem <- readValue(stream)
      _ <- skipWhitespaces(stream)
      hasNext <- hasNextValue(stream)
      _ = agg += elem
      res <-
        if hasNext then
          readAllImpl(skipWhitespaces, readValue, stream, agg)
        else
          Monad.pure(agg.toSeq)
        end if
    yield
      res
  end readAllImpl
}
