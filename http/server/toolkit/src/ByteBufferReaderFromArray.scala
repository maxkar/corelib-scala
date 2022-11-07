package io.github.maxkar
package http.server.toolkit

import fun.typeclass.Monad

import java.nio.ByteBuffer

import http.server.api.ByteArrayReader
import http.server.api.ByteBufferReader

/**
 * Reader of buffer data from the array reader.
 */
final class ByteBufferReaderFromArray[M[_]: Monad](peer: ByteArrayReader[M])
    extends ByteBufferReader[M]:

  /** Temporary buffer (if the actual buffer does not have backing array). */
  private var tmp: Array[Byte] = null

  override def read(buffer: ByteBuffer): M[Boolean] =
    if buffer.hasArray() then
      val arr = buffer.array()
      return peer.read(arr, buffer.position(), buffer.remaining()) map { rd =>
        buffer.position(rd + buffer.position())
        rd > 0
      }
    end if

    val tgt = allocateTmp()
    val limit = Math.min(buffer.remaining(), tgt.length)
    peer.read(tgt, 0, limit) map { rd =>
      if rd > 0 then
        buffer.put(tgt, 0, limit)
        true
      else
        false
    }
  end read


  /** Allocates temp buffer if it was not yet allocated. */
  private def allocateTmp(): Array[Byte] =
    if tmp == null then tmp = new Array[Byte](2048)
    tmp
  end allocateTmp
end ByteBufferReaderFromArray
