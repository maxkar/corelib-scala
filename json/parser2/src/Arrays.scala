package io.github.maxkar
package json.parser

import fun.Monad
import scala.collection.mutable.ArrayBuffer


/** Array readers and related utilities. */
object Arrays {

  /** Error encoding for array formats. */
  trait Errors[M[_], -S]:
    /**
     * Invoked when array start was expected but something different occured in the stream.
     * Stream position is before the character that was expected to be array start.
     */
    def invalidArrayStart[T](stream: S): M[T]

    /**
     * Invoked when array end or value separator was expected
     * but something different occured in the stream.
     * Stream position is before the character that should be array end or
     * value separator.
     */
    def invalidArrayEnd[T](stream: S): M[T]
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
  def readArrayStart[M[_]: Monad, S <: CharacterStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isArrayStart(lookAhead.charAt(0)) then
        errs.invalidArrayStart(stream)
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
  def hasNextValue[M[_]: Monad, S <: CharacterStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Boolean] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 then
        errs.invalidArrayEnd(stream)
      else
        lookAhead.charAt(0) match {
          case ',' => stream.skip(1) map { _ => true }
          case ']' => stream.skip(1) map { _ => false }
          case _ => errs.invalidArrayEnd(stream)
        }
    }
  end hasNextValue


  /**
   * Reads complete array as a sequence.
   * Whitespace and value reading is delegated to provided handlers.
   */
  def readAll[T, M[_]: Monad, S <: CharacterStream[M]](
        skipWhitespaces: S => M[Unit],
        readValue: S => M[T],
        stream: S,
      )(using
        errs: Errors[M, S]
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
  private def readAllImpl[T, M[_]: Monad, S <: CharacterStream[M]](
        skipWhitespaces: S => M[Unit],
        readValue: S => M[T],
        stream: S,
        agg: ArrayBuffer[T],
      )(using
        errs: Errors[M, S]
      ): M[Seq[T]] =
    for
      _ <- skipWhitespaces(stream)
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
