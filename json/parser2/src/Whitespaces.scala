package io.github.maxkar
package json.parser

import fun.Monad


/**
 * (Standard) Whitespace reader and utilities.
 */
object Whitespaces:
  /**
   * Returns a number of whitespaces in the sequence.
   */
  def countWhitespaces(cs: CharSequence): Int =
    var ptr = 0
    while ptr < cs.length() && isWhitespace(cs.charAt(ptr)) do
      ptr += 1
    ptr
  end countWhitespaces


  /**
   * Checks if the given character is considered to be whitespace.
   */
  def isWhitespace(chr: Char): Boolean =
    chr match
      case ' ' | '\t' | '\r' | '\n' => true
      case _ => false
    end match
  end isWhitespace


  /**
   * Reads a next portion of whitespaces from the data stream.
   * @return `null` if there are no more whitespaces or next portion of whitespace characters.
   */
  def next[M[_]: Monad](stream: CharacterStream[M]): M[CharSequence] =
    stream.peek(1) flatMap { lookAhead =>
      val wsCharCount = countWhitespaces(lookAhead)
      if wsCharCount == 0 then
        Monad.pure(null)
      else
        stream.consume(wsCharCount)
    }
  end next


  /**
   * Parses a next portion of whitespaces and returns flag indicating if it was the
   * final part of the input. This may be used to optimize happy-path scenario where all
   * whitespaces fits the buffer and could be read in one go.
   * @return pair of the next input portion and `hasMore` flag.
   */
  def parse[M[_]: Monad](stream: CharacterStream[M]): M[(CharSequence, Boolean)] =
    stream.peek(1) flatMap { lookAhead =>
      val wsCharCount = countWhitespaces(lookAhead)
      if wsCharCount == 0 then
        Monad.pure(("", false))
      else
        val mayHaveMore = wsCharCount >= lookAhead.length()
        stream.consume(wsCharCount) map { chars => (chars, mayHaveMore)}
      end if
    }
  end parse


  /**
   * Skips all whitespace characters in the input stream.
   */
  def skipAll[M[_]: Monad](stream: CharacterStream[M]): M[Unit] =
    next(stream) flatMap {
      case null => Monad.pure(())
      case _ => skipAll(stream)
    }



  /**
   * Reads all whitespaces characters as a string. This method may be memory-inefficient.
   */
  def readAll[M[_]: Monad](stream: CharacterStream[M]): M[String] =
    readAllImpl(new StringBuilder(), stream)


  /**
   * Reads data into the accumulator and then returns all accumulated data.
   * @param accum accumulator to read data into.
   */
  private def readAllImpl[M[_]: Monad](
        accum: StringBuilder,
        stream: CharacterStream[M])
      : M[String] =
    next(stream) flatMap { nextSection =>
      if nextSection == null then
        Monad.pure(accum.toString())
      else
        accum.append(nextSection)
        readAllImpl(accum, stream)
    }
  end readAllImpl

end Whitespaces
