package io.github.maxkar
package text.v2.input

import fun.typeclass.Monad
import io.github.maxkar.text.LocationInfo
import io.github.maxkar.text.Location

/**
 * Buffered look-ahead stream backed up by the input stream of the type `T`.
 */
final class BufferedLookAhead[+T] private(
    private val peer: T,
    private val buffer: LookAheadBuffer
) {
  /** A base stream (the one being buffered). */
  def baseStream: T = peer
}

object BufferedLookAhead {
  /** IO errors that may happen with the look ahead. */
  trait IOErrors[M[_]] {
    /** Look ahead requested is more than supported by the buffer. */
    def lookAheadTooBig[T](requested: Int, supported: Int): M[T]
  }

  object IOErrors {
    inline def lookAheadTooBig[M[_], T](
          requested: Int,
          supported: Int
        )(using
          errs: IOErrors[M]
        ): M[T] =
      errs.lookAheadTooBig(requested, supported)
  }


  /** Creates a new buffered look ahead. */
  def apply[T](stream: T, bufferSize: Int): BufferedLookAhead[T] =
    new BufferedLookAhead(stream, LookAheadBuffer(bufferSize))


  given locationInfo[M[_]: Monad, T]: LocationInfo[M, BufferedLookAhead[T]] with {
    override def getLocation(stream: BufferedLookAhead[T]): M[Location] =
      Monad.pure(stream.buffer.location)
  }


  /** Given instances for buffered look-ahead stream. */
  given bufferedOps[M[_]: Monad: IOErrors, T: ReadsIn[M]]: LookAhead[M, BufferedLookAhead[T]] with {
    override def fill(stream: BufferedLookAhead[T], request: Int): M[Int] = {
      if stream.buffer.size >= request || stream.buffer.isEof then
        Monad.pure(stream.buffer.size)
      else if stream.buffer.capacity < request then
        IOErrors.lookAheadTooBig(request, stream.buffer.capacity)
      else
          readPortion(stream) <+> fill(stream, request)
    }


    /** Reads a portion of data into the buffer. */
    private def readPortion(stream: BufferedLookAhead[T]): M[Unit] =
      stream.peer.read(stream.buffer.writeBuffer, stream.buffer.writeStart, stream.buffer.writeEnd) <| { readCount =>
        if readCount < 0 then
          stream.buffer.markEof()
        else
          stream.buffer.dataWritten(readCount)
      }


    override def peek(stream: BufferedLookAhead[T], offset: Int): M[Int] =
      stream.fill(offset + 1) <| { available =>
        if available <= offset then -1 else stream.buffer.lookAhead(offset)
      }


    override def read(
          stream: BufferedLookAhead[T],
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int
        ): M[Int] = {
      val buffer = stream.buffer

      def doRead(writePtr: Int = 0): M[Int] = {
        val bytesWritten = buffer.read(target, writePtr, targetEnd)
        val newPtr = writePtr + bytesWritten

        if newPtr == targetEnd || buffer.isEof then
          Monad.pure(newPtr - targetStart)
        else
          readPortion(stream) <+> doRead(newPtr)
      }
      doRead(targetStart)
    }


    override def skip(stream: BufferedLookAhead[T], count: Int): M[Unit] = {
      val toDrop = Math.min(count, stream.buffer.size)
      stream.buffer.skip(toDrop)

      val remaining = count - toDrop
      if remaining == 0 || stream.buffer.isEof then
        Monad.pure(())
      else
        readPortion(stream) <+> skip(stream, remaining)
    }


    override def readWhile(
          stream: BufferedLookAhead[T],
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int,
          predicate: Char => Boolean
        ): M[Int] = {
      val buffer = stream.buffer

      def doRead(writePtr: Int): M[Int] = {
        val written = buffer.readWhile(target, writePtr, targetEnd, predicate)

        val newWritePtr = written + writePtr
        if newWritePtr == targetEnd || buffer.size > 0 || buffer.isEof then
          Monad.pure(newWritePtr - targetStart)
        else
          readPortion(stream) <+> doRead(newWritePtr)
      }

      doRead(targetStart)
    }


    override def skipWhile(stream: BufferedLookAhead[T], predicate: Char => Boolean): M[Unit] = {
      stream.buffer.skipWhile(predicate)
      if stream.buffer.size > 0 || stream.buffer.isEof  then
        Monad.pure(())
      else
        readPortion(stream) <+> skipWhile(stream, predicate)
    }


    override def atEnd(stream: BufferedLookAhead[T]): M[Boolean] =
      peek(stream, 0) <| { _ < 0 }
  }
}
