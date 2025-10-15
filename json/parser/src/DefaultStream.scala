package io.github.maxkar
package json.parser

/**
 * Typeclass providing most common operations for reading data from
 * a stream `S`. This stream API is intended for use by the JSON
 * modules (json model readers) in the platform.
 */
trait DefaultStream[M[_], -S] extends SkipStream[M, S] {
  extension (stream: S) {
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
