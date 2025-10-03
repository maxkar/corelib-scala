package io.github.maxkar
package json.writer.v2

/**
 * Writer that supports specific JSON model type.
 *
 * @tparam M input/output operation type
 * @tparam T supported JSON model.
 */
abstract class JsonWriter[M[_], S, T] private[writer]() {
  /** Writes (JSON-like) value into the output stream. */
  def write(value: T, stream: S): M[Unit]
}
