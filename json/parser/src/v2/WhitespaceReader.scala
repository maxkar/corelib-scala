package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn

/** Reader for the whitespaces. */
opaque type WhitespaceReader[S] = S

object WhitespaceReader {
  /** Creates a new reader of whitespaces for the given stream. */
  inline def apply[S](base: S): WhitespaceReader[S] = base


  given reader[M[_], S: LooksAheadIn[M]]: Reader[M, WhitespaceReader[S]] with {
    override def read(source: WhitespaceReader[S], target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      source.readWhile(target, targetStart, targetEnd, isWhitespace)
  }


  extension [S](base: WhitespaceReader[S]) {
    def skipAll[M[_]]()(using la: LookAhead[M, S]): M[Unit] =
      base.skipWhile(isWhitespace)
  }


  /** Checks if the character is a whitespace character. */
  def isWhitespace(char: Char): Boolean =
    char match {
      case ' ' | '\t' | '\r' | '\n' => true
      case _ => false
    }
}

