package io.github.maxkar
package text.input.instances

import text.Location
import text.LocationTracker
import text.input.typeclass.Input
import text.input.typeclass.LookAhead
import text.input.typeclass.InputLocation

import fun.typeclass.Monad


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
  import BufferedInput.Buffer

  /** A type of the "buffered input stream". */
  opaque type Stream[T] = Buffer => M[T]

  given monad: Monad[Stream] with {
    override def pure[T](v: T): Stream[T] =
      _ => mmonad.pure(v)

    override def bind[S, R](v: Stream[S], fn: S => Stream[R]): Stream[R] =
      buffer =>
        v(buffer) <||| { r => (fn(r)(buffer)) }

    override def fmap[S, R](v: Stream[S], fn: S => R): Stream[R] =
      buffer => v(buffer) <| { fn }
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


  /** Implementation of the lookAhead method. */
  private def lookAheadImpl(offset: Int)(buffer: Buffer): M[Int] = {
      if offset <= buffer.size then
        mmonad.pure(buffer.charAt(offset))
      else if buffer.eof then
        mmonad.pure(-1)
      else
        requestLookAheadImpl(offset)(buffer) <||| { _ => lookAheadImpl(offset)(buffer) }
  }


  /** Implementation of the look-ahead algorithm. */
  private def requestLookAheadImpl(limit: Int)(buffer: Buffer): M[Int] = {
    if buffer.maxCapacity < limit then
      return lookAheadTooBig(limit, buffer.maxCapacity)

    /* We have enough characters OR we know we won't get more characters. */
    if buffer.eof || buffer.size > limit then
      return mmonad.pure(buffer.size)

    fillUpTo(buffer, limit)
  }


  private def readImpl(
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        readSoFar: Int = 0,
      )(
        buffer: Buffer
      ): M[Int] = {
    val copiedCount = buffer.readTo(target, targetStart, targetEnd)
    val newStart = targetStart + copiedCount

    /* We filled the buffer, return now. Or maybe we know there is no more
     * data to read. Also return now.
     */
    if newStart == targetEnd || buffer.eof then
      return mmonad.pure(readSoFar + copiedCount)

    readDirect(target, targetStart, targetEnd, readSoFar)(buffer)
  }


  /** Reads the data directly to the target, without buffering. */
  private def readDirect(
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        readSoFar: Int,
      )(
        buffer: Buffer
      ): M[Int] =
    input.read(target, targetStart, targetEnd) <||| { readCount =>
      if readCount < 0 then {
        buffer.eof = true
        mmonad.pure(readSoFar)
      } else {
        val newStart = targetStart + readCount
        val totalRead = readSoFar + readCount
        if newStart == readSoFar then
          mmonad.pure(totalRead)
        else
          readDirect(target, newStart, targetEnd, totalRead)(buffer)
      }
    }


  /** Fills the buffer up to the limit. */
  private def fillUpTo(buffer: Buffer, limit: Int): M[Int] =
    input.read(buffer.buffer, buffer.writeStart, buffer.writeEnd) <||| { readCount =>
      if readCount < 0 then {
        buffer.eof = true
        mmonad.pure(buffer.size)
      } else {
        buffer.newDataAdded(readCount)
        if buffer.size >= limit then
          mmonad.pure(buffer.size)
        else
          fillUpTo(buffer, limit)
      }
    }


  private def skipImpl(count: Int)(buffer: Buffer): M[Unit] = {
    if buffer.size >= count then {
      buffer.skip(count)
      return mmonad.pure(())
    }

    buffer.clear()
    val toRead = buffer.size - count
    dropFromStream(toRead, buffer)
  }


  /** Drops the requested amount of bytes from the buffer. */
  private def dropFromStream(dropCount: Int, buffer: Buffer): M[Unit] =
    input.read(buffer.buffer, 0, Math.min(dropCount, buffer.buffer.length)) <||| { readCount =>
      if readCount < 0 then
        mmonad.pure(())
      else {
        val remaining = dropCount - readCount
        if remaining == 0 then
          mmonad.pure(())
        else
          dropFromStream(remaining, buffer)
      }
    }

  /** Reads data from the buffer. */
  private def readWhileImpl(
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        predicate: Char => Boolean,
        totalRead: Int = 0,
      )(
        buffer: Buffer,
      ): M[Int] = {
    val readCount = buffer.readWhile(target, targetStart, targetEnd, predicate)
    val newReadCount = totalRead + readCount
    /* There is some data in the buffer. This means the predicate was not satisfied. */
    if buffer.size > 0 then
      return mmonad.pure(newReadCount)

    /* Reset the buffer to simplify reading. */
    buffer.clear()
    input.read(buffer.buffer, 0, buffer.buffer.length) <||| { readCount =>
      if readCount < 0 then {
        buffer.eof = true
        mmonad.pure(newReadCount)
      } else {
        buffer.newDataAdded(readCount)
        readWhileImpl(target, targetStart + readCount, targetEnd, predicate, newReadCount)(buffer)
      }
    }
  }


  private def skipWhileImpl(
        predicate: Char => Boolean,
        totalRead: Int = 0
      )(
        buffer: Buffer
      ): M[Unit] = {
    val readCount = buffer.dropWhile(predicate)
    /* There is some data in the buffer. This means the predicate was not satisfied. */
    if buffer.size > 0 then
      return mmonad.pure(())

    /* Reset the buffer to simplify reading. */
    buffer.clear()
    input.read(buffer.buffer, 0, buffer.buffer.length) <||| { readCount =>
      if readCount < 0 then {
        buffer.eof = true
        mmonad.pure(())
      } else {
        buffer.newDataAdded(readCount)
        skipWhileImpl(predicate)(buffer)
      }
    }
  }
}


object BufferedInput {
  /**
   * Buffer for input/output operations. Implemented as a cyclic buffer.
   *
   * @param buffer buffer used for holding data.
   * @param maxCapacity maximal capacity of the buffer.
   */
  private final class Buffer(
      var buffer: Array[Char],
  ) {
    /** The "read offset" in the buffer. */
    var offset: Int = 0

    /** Size (numbef of available characters) in the buffer. */
    var size: Int = 0

    /** If end-of-file was reached during the reading. */
    var eof: Boolean = false

    /** Current location (location of the next character to be read). */
    var location: Location = new Location(0, 1, 1)


    /** Position where write to the buffer should start. */
    def writeStart: Int = {
      val target = offset + size
      if target < buffer.length then
        target
      else
        target - buffer.length
    }


    /** Position where write to the buffer should end. */
    def writeEnd: Int = {
      val writeStart = offset + size
      if writeStart < buffer.length then
        buffer.length
      else
        offset
    }


    /** Maximal capacity of this buffer. */
    def maxCapacity: Int = buffer.length


    /** Updates the buffer status after some characters were added. */
    def newDataAdded(readCount: Int): Unit =
      size += readCount


    /** Retrieves character at the given offset. */
    def charAt(charOffset: Int): Int = {
      val arrayOffset = offset + charOffset
      val trueOffset = if arrayOffset >= buffer.length then arrayOffset - buffer.length else arrayOffset
      buffer(trueOffset)
    }


    /** Reads data into the target buffer and returns the number of characters copied. */
    def readTo(target: Array[Char], targetStart: Int, targetEnd: Int): Int = {
      val targetSize = targetEnd - targetStart
      val tailRead = Math.min(Math.min(buffer.length - offset, size), targetSize)

      System.arraycopy(buffer, offset, target, targetStart, tailRead)
      offset += tailRead
      size -= tailRead
      if offset >= buffer.length then
        offset = 0

      if tailRead == targetSize then
        return tailRead

      val headRead = Math.min(targetSize - tailRead, size)
      System.arraycopy(buffer, 0, target, targetStart + tailRead, headRead)
      offset = headRead
      size -= headRead

      return tailRead + headRead
    }

    /** Skips (drops) the requested number of characters from this buffer. */
    def skip(count: Int): Unit = {
      size -= count
      offset += count
      if offset >= buffer.length then
        offset -= buffer.length
    }


    /** Clears the buffer and discards all the data. */
    def clear(): Unit = {
      offset = 0
      size = 0
    }

    /** Reads data from the buffer. */
    def readWhile(
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int,
          predicate: Char => Boolean,
        ): Int = {
      var outPtr = targetStart
      var inPtr = offset

      while outPtr < targetEnd && size > 0 do {
        val nextChar = buffer(inPtr)
        if !predicate(nextChar) then {
          offset = inPtr
          return outPtr - targetStart
        }
        target(outPtr) = nextChar

        outPtr += 1
        inPtr += 1
        if (inPtr == buffer.length) {
          inPtr = 0
        }
        size -= 1
      }

      offset = inPtr
      return outPtr - targetStart
    }


    /** Drops data from the buffer. */
    def dropWhile(
          predicate: Char => Boolean,
        ): Unit = {
      var initialSize = size
      var inPtr = offset

      while size > 0 do {
        val nextChar = buffer(inPtr)
        if !predicate(nextChar) then {
          offset = inPtr
          return
        }

        inPtr += 1
        if (inPtr == buffer.length) {
          inPtr = 0
        }
        size -= 1
      }

      offset = inPtr
      return
    }
  }
}
