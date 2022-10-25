package io.github.maxkar
package json.writer

import text.output.Stream

/**
 * Writer that supports specific JSON model type.
 *
 * @tparam M input/output operation type
 * @tparam T supported JSON model.
 */
abstract class Writer[M[_], T] private[writer]():
  /** Writes (JSON-like) value into the output stream. */
  def write(value: T, stream: Stream[M]): M[Unit]
end Writer
