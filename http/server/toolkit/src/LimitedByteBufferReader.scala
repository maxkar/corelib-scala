package io.github.maxkar
package http.server.toolkit

import fun.typeclass.Monad

import java.nio.ByteBuffer

import http.server.api.ByteBufferReader

/**
 * An array reader that raises an exception if too much bytes
 * could be read from the `peer` stream.
 *
 * @param peer stream to read from.
 * @param limit limit after which to raise an exception.
 * @param tooMuch a function to invoke when too much data was received.
 */
final class LimitedByteBufferReader[M[_]: Monad](
      peer: ByteBufferReader[M],
      private var limit: Long,
      tooMuch: () => M[Boolean],
    ) extends ByteBufferReader[M]:

  override def read(target: ByteBuffer): M[Boolean] =
    if limit < 0 then
      return tooMuch()

    val start = target.position()
    peer.read(target) flatMap { hasMore =>
      limit -= (target.position() - start)
      if limit < 0 then
        tooMuch()
      else
        Monad.pure(hasMore)
    }
  end read

end LimitedByteBufferReader
