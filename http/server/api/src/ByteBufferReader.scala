package io.github.maxkar
package http.server.api

import java.nio.ByteBuffer

/** Reader that puts read data into the byte buffer. */
trait ByteBufferReader[M[_]]:
  /**
   * Reads bytes into the buffer.
   * @return `true` if more data may be expected and false otherwise (i.e. when
   *   end of stream is reached).
   */
  def read(buffer: ByteBuffer): M[Boolean]
end ByteBufferReader
