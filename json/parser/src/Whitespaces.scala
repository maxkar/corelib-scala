package io.github.maxkar
package json.parser

object Whitespaces {
  /** Factory for the whitespace representation `W` read from the stream `S`. */
  trait Factory[M[_], -S, W] {
    /** Consumes whitespaces while predicate is satisfied and returns its representation. */
    def skipWhile(stream: S, predicate: Char => Boolean): M[W]
  }


  /** Reads whitespaces from the stream using the provided factory. */
  def read[M[_], S, W](stream: S)(using factory: Factory[M, S, W]): M[W] =
    factory.skipWhile(stream, isWhitespace)


  /** Skips whitespaces in the stream. */
  def skip[M[_], S: SkipStream.In[M]](stream: S): M[Unit] =
    stream.skipWhile(isWhitespace)


  /** Checks if the character is a whitespace character. */
  def isWhitespace(char: Char): Boolean =
    char match {
      case ' ' | '\t' | '\r' | '\n' => true
      case _ => false
    }
}
