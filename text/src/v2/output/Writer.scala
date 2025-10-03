package io.github.maxkar
package text.v2.output

import fun.typeclass.Applicative
import scala.annotation.targetName

/**
 * Writer into the "stream" of type `T` in the execution monad `M``.
 */
trait Writer[M[_], -T] {
  /** Outputs data into the stream. */
  def write(out: T, data: CharSequence): M[Unit]

  extension (t: T) {
    @targetName("writeExt")
    inline def write(data: CharSequence): M[Unit] =
      this.write(t, data)
  }
}


object Writer {
  type For[T] = [M[_]] =>> Writer[M, T]
  type In[M[_]] = [T] =>> Writer[M, T]

  given StringBuilderWriter[M[_]](using app: Applicative[M]): Writer[M, StringBuilder] with {
    override def write(out: StringBuilder, data: CharSequence): M[Unit] = {
      out.append(data)
      app.pure(())
    }
  }
}
