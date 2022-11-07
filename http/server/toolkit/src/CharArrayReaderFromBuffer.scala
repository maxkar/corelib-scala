package io.github.maxkar
package http.server.toolkit

import fun.typeclass.Monad

import java.nio.CharBuffer

import http.server.api.CharArrayReader
import http.server.api.CharBufferReader

/**
 * Adapter from buffer reader to array reader.
 */
final class CharArrayReaderFromBuffer[M[_]: Monad](
      peer: CharBufferReader[M],
    )
    extends CharArrayReader[M]:
  /** Set when EOF was reached. */
  private var eof = false


  override def read(target: Array[Char], start: Int, length: Int): M[Int] =
    if eof then
      return Monad.pure(-1)

    val buf = CharBuffer.wrap(target, start, length)
    peer.read(buf) map { hasMore =>
      if !hasMore then
        eof = true

      /* Any data was read. */
      if buf.position() > 0 then
        buf.position()
      else
        -1
    }
end CharArrayReaderFromBuffer
