package io.github.maxkar
package http.server.api

import java.nio.CharBuffer

/** Reader that puts read data into the character buffer. */
trait CharBufferReader[M[_]]:
  /** Reads bytes into the buffer.
   * @return `true` if more data may be expected and false otherwise (i.e. when
   *   end of stream is reached).
   */
  def read(buffer: CharBuffer): M[Boolean]
end CharBufferReader
