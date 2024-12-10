package io.github.maxkar
package text.output

import fun.typeclass.Applicative

/**
 * Stream implementation that collects all provided data into the internal
 * buffer (string builder) and could later return it as a stream.
 */
final class StringBuilderStream[M[_]: Applicative] extends Stream[M] {
  /** Internal bufer for all the data written. */
  private val buffer = new StringBuilder()

  /** Cached no-op value. */
  private val pass = Applicative.pure(())

  override def write(data: CharSequence): M[Unit] = {
    buffer.append(data)
    pass
  }

  /** Returns all the output collected so far. */
  def data: String = buffer.toString()
}
