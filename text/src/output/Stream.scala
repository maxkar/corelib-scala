package io.github.maxkar
package text.output


/**
 * Simple output stream for text data.
 * @tparam M type of the operation monad.
 */
trait Stream[M[_]]:
  /** Outputs data into the stream. */
  def write(data: CharSequence): M[Unit]
end Stream
