package io.github.maxkar
package json.writer

import fun.typeclass.Applicative
import java.io.Writer

/**
 * Writer providing common functionality used by the JSON modules.
 */
trait DefaultWriter[M[_], -S] extends CharWriter[M, S] {
  extension (stream: S) {
    /** Outputs the complete character sequence into the stream. */
    def write(cs: CharSequence): M[Unit] = write(cs, 0, cs.length())
    /** Outputs part of the character sequence. */
    def write(cs: CharSequence, start: Int, end: Int): M[Unit]
  }
}

object DefaultWriter {
  type In[M[_]] = [T] =>> DefaultWriter[M, T]
  type Of[T] = [M[_]] =>> DefaultWriter[M, T]


  given javaIoWriter[M[_]: Applicative]: DefaultWriter[M, java.io.Writer] with {
    extension (stream: java.io.Writer) {
      override def write(cs: CharSequence, start: Int, end: Int): M[Unit] =
        Applicative.pure(stream.append(cs, start, end))
      override def write(c: Char): M[Unit] =
        Applicative.pure(stream.write(c))
    }
  }
}
