package io.github.maxkar
package text.input.instances

import text.Location
import text.LocationTracker
import text.input.typeclass.Input
import text.input.typeclass.LookAhead
import text.input.typeclass.InputLocation

import fun.typeclass.Monad
import io.github.maxkar.text.input.typeclass.LookAhead.skip


/**
 * Implementation of the buffered input module transformer for the given monad
 * and result type.
 *
 * @param lookAheadTooBig function to raise an error when requested look-ahead
 *   is too big for the given capacity.
 */
final class BufferedInput[M[_], T](
        lookAheadTooBig: [T] => (Int, Int) => M[T]
      )(using
        mmonad: Monad[M],
        input: Input[M],
      ) {
  /** A type of the "buffered input stream". */
  opaque type Stream[T] = LookAheadBuffer => M[T]

  given monad: Monad[Stream] with {
    override def pure[T](v: T): Stream[T] =
      _ => mmonad.pure(v)

    override def bind[S, R](v: Stream[S], fn: S => Stream[R]): Stream[R] =
      buffer =>
        v(buffer) <||| { r => (fn(r)(buffer)) }

    override def fmap[S, R](v: Stream[S], fn: S => R): Stream[R] =
      buffer => v(buffer) <| fn
  }


  given location: InputLocation[Stream[Location]] with {
    override def get(): Stream[Location] =
      buffer => mmonad.pure(buffer.location)
  }


  given bufferedInput: LookAhead[Stream] with {
    override def requestLookAhead(limit: Int): Stream[Int] = requestLookAheadImpl(limit)
    override def lookAhead(offset: Int): Stream[Int] = lookAheadImpl(offset)
    override def read(target: Array[Char], targetStart: Int, targetEnd: Int): Stream[Int] =
      readImpl(target, targetStart, targetEnd)
    override def skip(count: Int): Stream[Unit] = skipImpl(count)
    override def readWhile(
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int,
          predicate: Char => Boolean
        ): Stream[Int] = readWhileImpl(target, targetStart, targetEnd, predicate)
    override def skipWhile(predicate: Char => Boolean): Stream[Unit] = skipWhileImpl(predicate)
  }


  /** Implementation of the look-ahead algorithm. */
  private def requestLookAheadImpl(limit: Int)(buffer: LookAheadBuffer): M[Int] = {
    if buffer.capacity < limit then
      return lookAheadTooBig(limit, buffer.capacity)

    fillUpTo(limit)(buffer)
  }


  /** Fills the buffer up to the limit. */
  private def fillUpTo(limit: Int)(buffer: LookAheadBuffer): M[Int] = {
    if buffer.isEof || buffer.size >= limit then
      return mmonad.pure(buffer.size)
    readMoreData(buffer) <||| fillUpTo(limit)
  }


  /** Populates buffer with more data. */
  private def readMoreData(buffer: LookAheadBuffer): M[LookAheadBuffer] =
    input.read(buffer.writeBuffer, buffer.writeStart, buffer.writeEnd) <| { readCount =>
      if readCount < 0 then
        buffer.markEof()
      buffer
    }


  /** Implementation of the lookAhead method. */
  private def lookAheadImpl(offset: Int)(buffer: LookAheadBuffer): M[Int] = {
      if offset <= buffer.size then
        mmonad.pure(buffer.lookAhead(offset))
      else if buffer.isEof then
        mmonad.pure(-1)
      else
        requestLookAheadImpl(offset)(buffer) <||| { _ => lookAheadImpl(offset)(buffer) }
  }


  private def readImpl(
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        readSoFar: Int = 0,
      )(
        buffer: LookAheadBuffer
      ): M[Int] = {
    if buffer.isEof then
      return mmonad.pure(readSoFar)

    val copiedCount = buffer.read(target, targetStart, targetEnd)
    val newStart = targetStart + copiedCount

    /* We filled the buffer, return now. Or maybe we know there is no more
     * data to read. Also return now.
     */
    if newStart == targetEnd then
      return mmonad.pure(readSoFar + copiedCount)

    readMoreData(buffer) <||| readImpl(target, newStart, targetEnd, readSoFar + copiedCount)
  }


  private def skipImpl(count: Int)(buffer: LookAheadBuffer): M[Unit] = {
    if buffer.isEof then
      return mmonad.pure(())

    if buffer.size >= count then {
      buffer.skip(count)
      return mmonad.pure(())
    }

    val dropped = buffer.size
    readMoreData(buffer) <||| skipImpl(count - dropped)
  }


  /** Reads data from the buffer. */
  private def readWhileImpl(
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        predicate: Char => Boolean,
        totalRead: Int = 0,
      )(
        buffer: LookAheadBuffer,
      ): M[Int] = {
    if buffer.isEof then
      return mmonad.pure(totalRead)

    val readCount = buffer.readWhile(target, targetStart, targetEnd, predicate)
    val newReadCount = totalRead + readCount

    /* There is some data in the buffer. This means the predicate was not satisfied. */
    if buffer.size > 0 then
      return mmonad.pure(newReadCount)

    val newStart = targetStart + readCount
    if newStart == targetEnd then
      return mmonad.pure(newReadCount)

    readMoreData(buffer) <||| readWhileImpl(target, newStart, targetEnd, predicate, totalRead)
  }


  private def skipWhileImpl(
        predicate: Char => Boolean,
        totalRead: Int = 0
      )(
        buffer: LookAheadBuffer
      ): M[Unit] = {
    if buffer.isEof then
      return mmonad.pure(())

    val readCount = buffer.skipWhile(predicate)
    /* There is some data in the buffer. This means the predicate was not satisfied. */
    if buffer.size > 0 then
      return mmonad.pure(())

    readMoreData(buffer) <||| skipWhileImpl(predicate)
  }
}
