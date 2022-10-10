package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Monad
import scala.collection.mutable.HashMap


/** Json object reader and related utilities. */
object Objects:
  /** Error encoding for object formats. */
  trait Errors[M[_]]:
    /** Encodes invalid object start. */
    def invalidObjectStart[T](): M[T]

    /**
     * Encodes situation where value separator or object end character
     * is expected but no such element is found.
     */
    def entrySeparatorOrEndIsMissing[T](): M[T]

    /**
     * Encodes situation where key-value separator was expected but
     * no such character occured in the stream.
     */
    def invalidKeyValueSeparator[T](): M[T]
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
  def readObjectStart[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Unit] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isObjectStart(lookAhead.charAt(0)) then
        implicitly[Errors[M]].invalidObjectStart()
      else
        stream.skip(1)
    }
  end readObjectStart


  /**
   * Checks if object has first value (after object start was read). Consumes
   * the object terminator if object is empty.
   */
  def hasFirstValue[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Boolean] =
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
  def hasNextValue[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Boolean] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 then
        implicitly[Errors[M]].entrySeparatorOrEndIsMissing()
      else
        lookAhead.charAt(0) match {
          case ',' => stream.skip(1) map { _ => true }
          case '}' => stream.skip(1) map { _ => false }
          case _ => implicitly[Errors[M]].entrySeparatorOrEndIsMissing()
        }
    }
  end hasNextValue


  /** Reads key-value separator. */
  def readKeyValueSeparator[M[_]: Monad: Errors](stream: CharacterStream[M]): M[Unit] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 || !isKeyValueSeparator(lookAhead.charAt(0)) then
        implicitly[Errors[M]].invalidKeyValueSeparator()
      else
        stream.skip(1)
    }
  end readKeyValueSeparator


  /** Reads the complete object as a map. Duplicate keys will retain only the last value. */
  def readAll[K, V, M[_]: Monad: Errors](
          skipWhitespaces: CharacterStream[M] => M[Unit],
          readKey: CharacterStream[M] => M[K],
          readValue: CharacterStream[M] => M[V],
          stream: CharacterStream[M],
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
  private def readAllImpl[K, V, M[_]: Monad: Errors](
          skipWhitespaces: CharacterStream[M] => M[Unit],
          readKey: CharacterStream[M] => M[K],
          readValue: CharacterStream[M] => M[V],
          stream: CharacterStream[M],
          agg: HashMap[K, V],
      ): M[Map[K, V]] =
    for
      key <- readKey(stream)
      _ <- skipWhitespaces(stream)
      _ <- readKeyValueSeparator(stream)
      _ <- skipWhitespaces(stream)
      value <- readValue(stream)
      _ = agg.put(key, value)
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

