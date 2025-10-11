package io.github.maxkar
package json.writer.v3

/** Writer capable of writing a single character. */
trait CharWriter[M[_], -S] {
  extension (stream: S) {
    /** Outputs the signle character into the stream. */
    def write(chr: Char): M[Unit]
  }
}

object CharWriter {
  type In[M[_]] = [T] =>> CharWriter[M, T]
  type Of[T] = [M[_]] =>> CharWriter[M, T]
}
