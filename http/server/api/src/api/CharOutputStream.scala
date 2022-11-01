package io.github.maxkar
package http.server.api

import java.nio.CharBuffer

/** Writer of stream of characters. */
trait CharOutputStream[M[_]]:
  /** Writes buffer data into the output stream. */
  def write(data: CharBuffer): M[Unit]
end CharOutputStream


