package io.github.maxkar
package text.v2.input

import fun.typeclass.Monad
import io.github.maxkar.text.LocationInfo
import io.github.maxkar.text.Location

/**
 * Buffered look-ahead stream backed up by the input stream of the type `S`.
 */
final class BufferedLookAhead[+S] private(
    private val peer: S,
    private val buffer: LookAheadBuffer
) {
  /** A base stream (the one being buffered). */
  def baseStream: S = peer
}


object BufferedLookAhead {
  /** IO errors that may happen with the look ahead. */
  trait IOErrors[M[_], S] {
    /** Look ahead requested is more than supported by the buffer. */
    def lookAheadTooBig[T](stream: BufferedLookAhead[S], requested: Int, supported: Int): M[T]
  }
  type IOErrorsIn[M[_]] = [S] =>> IOErrors[M, S]


  object IOErrors {
    /** Creates an error handler that fails execution with the given message. */
    def raise[M[_], S](raiseFn: [T] => (BufferedLookAhead[S], String) => M[T]): IOErrors[M, S] =
      new IOErrors[M, S] {
        override def lookAheadTooBig[T](stream: BufferedLookAhead[S], requested: Int, supported: Int): M[T] =
          raiseFn(stream, s"Requested look-ahead ${requested} is more than the max supported ${supported}")
      }
  }

  /** Creates a new buffered look ahead. */
  def apply[S](stream: S, bufferSize: Int): BufferedLookAhead[S] =
    new BufferedLookAhead(stream, LookAheadBuffer(bufferSize))


  given locationInfo[M[_]: Monad, S]: LocationInfo[M, BufferedLookAhead[S]] with {
    override def getLocation(stream: BufferedLookAhead[S]): M[Location] =
      Monad.pure(stream.buffer.location)
  }


  /** Given instances for buffered look-ahead stream. */
  given bufferedOps[M[_]: Monad, S: ReadsIn[M]: IOErrorsIn[M]]: LookAhead[M, BufferedLookAhead[S]] with {
    override def fill(stream: BufferedLookAhead[S], request: Int): M[Int] = {
      if stream.buffer.size >= request || stream.buffer.isEof then
        Monad.pure(stream.buffer.size)
      else if stream.buffer.capacity < request then
        summon[IOErrors[M, S]].lookAheadTooBig(stream, request, stream.buffer.capacity)
      else
        readPortion(stream) <+> fill(stream, request)
    }


    /** Reads a portion of data into the buffer. */
    private def readPortion(stream: BufferedLookAhead[S]): M[Unit] =
      stream.peer.read(stream.buffer.writeBuffer, stream.buffer.writeStart, stream.buffer.writeEnd) <| { readCount =>
        if readCount < 0 then
          stream.buffer.markEof()
        else
          stream.buffer.dataWritten(readCount)
      }


    override def peek(stream: BufferedLookAhead[S], offset: Int): M[Int] =
      stream.fill(offset + 1) <| { available =>
        if available <= offset then -1 else stream.buffer.lookAhead(offset)
      }


    override def read(
          stream: BufferedLookAhead[S],
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


    override def skip(stream: BufferedLookAhead[S], count: Int): M[Unit] = {
      val toDrop = Math.min(count, stream.buffer.size)
      stream.buffer.skip(toDrop)

      val remaining = count - toDrop
      if remaining == 0 || stream.buffer.isEof then
        Monad.pure(())
      else
        readPortion(stream) <+> skip(stream, remaining)
    }


    override def readWhile(
          stream: BufferedLookAhead[S],
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


    override def skipWhile(stream: BufferedLookAhead[S], predicate: Char => Boolean): M[Unit] = {
      stream.buffer.skipWhile(predicate)
      if stream.buffer.size > 0 || stream.buffer.isEof  then
        Monad.pure(())
      else
        readPortion(stream) <+> skipWhile(stream, predicate)
    }


    override def atEnd(stream: BufferedLookAhead[S]): M[Boolean] =
      peek(stream, 0) <| { _ < 0 }
  }
}
