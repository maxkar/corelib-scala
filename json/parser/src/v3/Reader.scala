package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad

/** Reader typeclass for the stream of type `S`. */
trait Reader[M[_], -S] {
  extension (stream: S) {
    /**
     * Reads data from the `stream` into the `into` array.
     * @param into destination array.
     * @param offset destination offset.
     * @param length maximal length to read.
     * @return number of characters read or negative value if end of stream was reached.
     */
    def read(into: Array[Char], offset: Int, length: Int): M[Int]
  }
}

object Reader {
  type In[M[_]] = [T] =>> Reader[M, T]
  type Of[T] = [M[_]] =>> Reader[M, T]

  given javaIoReaderReader[M[_]: Monad]: Reader[M, java.io.Reader] with {
    extension (stream: java.io.Reader) {
      override def read(into: Array[Char], offset: Int, length: Int): M[Int] =
        Monad.pure(stream.read(into, offset, length))
    }
  }
}
