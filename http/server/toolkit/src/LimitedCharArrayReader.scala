package io.github.maxkar
package http.server.toolkit

import fun.typeclass.Monad

import http.server.api.CharArrayReader

/**
 * An array reader that raises an exception if too much bytes
 * could be read from the `peer` stream.
 *
 * @param peer stream to read from.
 * @param limit limit after which to raise an exception.
 * @param tooMuch a function to invoke when too much data was received.
 */
final class LimitedCharArrayReader[M[_]: Monad](
      peer: CharArrayReader[M],
      private var limit: Long,
      tooMuch: () => M[Int],
    ) extends CharArrayReader[M]:

  override def read(target: Array[Char], start: Int, length: Int): M[Int] =
    if limit < 0 then
      return tooMuch()

    peer.read(target, start, length) flatMap { readSize =>
      if readSize < 0 then
        Monad.pure(readSize)
      else
        limit -= readSize
        if limit < 0 then
          tooMuch()
        else
          Monad.pure(readSize)
      end if
    }
  end read
end LimitedCharArrayReader
