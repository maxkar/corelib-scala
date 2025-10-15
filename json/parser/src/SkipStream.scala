package io.github.maxkar
package json.parser

/** A typeclass for a reader that could skip characters. */
trait SkipStream[M[_], -S] extends Peek[M, S] {
  extension (stream: S) {
    /** Skips `count` characters from the input. */
    def skip(count: Int): M[Unit]

    /**
     * Skips characters matching the predicate and stops at
     * a character not matching it.
     */
    def skipWhile(predicate: Char => Boolean): M[Unit]
  }
}


object SkipStream {
  type In[M[_]] = [T] =>> SkipStream[M, T]
  type Of[T] = [M[_]] =>> SkipStream[M, T]
}
