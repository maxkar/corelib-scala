package io.github.maxkar
package http.server.api

/** Writer of stream of characters. */
trait CharOutputStream[M[_]]:
  /** Writes buffer data into the output stream. */
  def write(data: CharSequence): M[Unit]
end CharOutputStream
