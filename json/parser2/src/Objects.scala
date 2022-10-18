package io.github.maxkar
package json.parser

import fun.typeclass.Monad
import scala.collection.mutable.HashMap


/** Json object reader and related utilities. */
object Objects:
  /**
   * Error handlers for JSON object format.
   * @tparam M execution monad.
   * @tparam S type of the stream (i.e. "context") required by the error generation.
   */
  trait Errors[M[_], -S]:
    /**
     * Invoked when object start was expected but something different occured in the stream.
     * Stream position is before the character that was expected to be object start.
     */
    def invalidObjectStart[T](stream: S): M[T]

    /**
     * Invoked when object end or object entry separator was expected
     * but something different occured in the stream.
     * Stream position is before the character that should be object end or object entity
     * separator.
     */
    def invalidObjectEnd[T](stream: S): M[T]

    /**
     * Invoked when key-value separator was expected but something different occured on the stream.
     * Stream position is before the character that should be key-value separator.
     */
    def invalidKeyValueSeparator[T](stream: S): M[T]
  end Errors


  /** Checks if the haracter is valid object start character. */
  def isObjectStart(char: Char): Boolean =
    char == '{'

  /** Checks if the haracter is valid object end character. */
  def isObjectEnd(char: Char): Boolean =
    char == '}'

  /** Checks if the character is valid separator between two key-value pairs. */
  def isEntrySeparator(char: Char): Boolean =
    char == ','

  /** Checks if the character is valid key-value separator. */
  def isKeyValueSeparator(char: Char): Boolean =
    char == ':'


  /** Reads the object start character from the stream. */
  def readObjectStart[M[_]: Monad, S <: CharacterStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isObjectStart(lookAhead.charAt(0)) then
        errs.invalidObjectStart(stream)
      else
        stream.skip(1)
    }
  end readObjectStart


  /**
   * Checks if object has first value (after object start was read). Consumes
   * the object terminator if object is empty.
   */
  def hasFirstValue[M[_]: Monad, S <: CharacterStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Boolean] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isObjectEnd(lookAhead.charAt(0)) then
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
        errs.invalidObjectEnd(stream)
      else
        lookAhead.charAt(0) match {
          case ',' => stream.skip(1) map { _ => true }
          case '}' => stream.skip(1) map { _ => false }
          case _ => errs.invalidObjectEnd(stream)
        }
    }
  end hasNextValue


  /** Reads key-value separator. */
  def readKeyValueSeparator[M[_]: Monad, S <: CharacterStream[M]](
        stream: S
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isKeyValueSeparator(lookAhead.charAt(0)) then
        errs.invalidKeyValueSeparator(stream)
      else
        stream.skip(1)
    }
  end readKeyValueSeparator


  /** Reads the complete object as a map. Duplicate keys will retain only the last value. */
  def readAll[K, V, M[_]: Monad, S <: CharacterStream[M]](
          skipWhitespaces: S => M[Unit],
          readKey: S => M[K],
          readValue: S => M[V],
          stream: S,
      )(using
        errs: Errors[M, S]
      ): M[Map[K, V]] =
    for
      _ <- readObjectStart(stream)
      _ <- skipWhitespaces(stream)
      nonEmpty <- hasFirstValue(stream)
      res <-
        if nonEmpty then
          readAllImpl(skipWhitespaces, readKey, readValue, stream, new HashMap())
        else
          Monad.pure(Map.empty)
        end if
    yield res
  end readAll


  /** Aggregating reader implementation. */
  private def readAllImpl[K, V, M[_]: Monad, S <: CharacterStream[M]](
          skipWhitespaces: S => M[Unit],
          readKey: S => M[K],
          readValue: S => M[V],
          stream: S,
          agg: HashMap[K, V],
      )(using
        errs: Errors[M, S]
      ): M[Map[K, V]] =
    for
      _ <- skipWhitespaces(stream)
      key <- readKey(stream)
      _ <- skipWhitespaces(stream)
      _ <- readKeyValueSeparator(stream)
      _ <- skipWhitespaces(stream)
      value <- readValue(stream)
      _ = agg.put(key, value)
      _ <- skipWhitespaces(stream)
      hasNext <- hasNextValue(stream)
      res <-
        if hasNext then
          readAllImpl(skipWhitespaces, readKey, readValue, stream, agg)
        else
          Monad.pure(agg.toMap)
        end if
    yield res
  end readAllImpl
end Objects

