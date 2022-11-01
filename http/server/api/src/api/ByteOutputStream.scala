package io.github.maxkar
package http.server.api

/**
 * Writer of stream of bytes.
 */
trait ByteOutputStream[M[_]]:
  /** Writes the given portion of the array into the output stream. */
  def write(data: Array[Byte], offset: Int, length: Int): M[Unit]

  /** Writes the given portion of the array into the output stream. */
  final def write(data: Array[Byte], length: Int): M[Unit] =
    write(data, 0, length)

  /** Writes the whole array into the output stream. */
  final def write(data: Array[Byte]): M[Unit] =
    write(data, 0, data.length)
end ByteOutputStream
