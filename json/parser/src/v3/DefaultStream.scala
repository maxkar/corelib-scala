package io.github.maxkar
package json.parser.v3

/**
 * Typeclass providing most common operations for reading data from
 * a stream `S`. This stream API is intended for use by the JSON
 * modules (json model readers) in the platform.
 */
trait DefaultStream[M[_], -S] {
  extension (stream: S) {
    /** Skips `count` characters from the input. */
    def skip(count: Int): M[Unit]

    /**
     * Skips characters matching the predicate and stops at
     * a character not matching it.
     */
    def skipWhile(predicate: Char => Boolean): M[Unit]

    /**
     * Reads characters matching the predicate into the `into` buffer.
     * Stops at a first character not matching the predicate.
     */
    def readWhile(into: StringBuilder, predicate: Char => Boolean): M[Unit]
  }
}

object DefaultStream {
  type In[M[_]] = [T] =>> DefaultStream[M, T]
  type Of[T] = [M[_]] =>> DefaultStream[M, T]
}
