package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn

/** Reader for the whitespaces. */
opaque type WhitespaceReader[T] = T

object WhitespaceReader {
  /** Creates a new reader of whitespaces for the given stream. */
  inline def apply[T](base: T): WhitespaceReader[T] = base


  given reader[M[_], T: LooksAheadIn[M]]: Reader[M, WhitespaceReader[T]] with {
    override def read(source: WhitespaceReader[T], target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      source.readWhile(target, targetStart, targetEnd, isWhitespace)
  }


  extension [T](base: WhitespaceReader[T]) {
    def skipAll[M[_]]()(using la: LookAhead[M, T]): M[Unit] =
      base.skipWhile(isWhitespace)
  }


  /** Checks if the character is a whitespace character. */
  def isWhitespace(char: Char): Boolean =
    char match {
      case ' ' | '\t' | '\r' | '\n' => true
      case _ => false
    }
}

