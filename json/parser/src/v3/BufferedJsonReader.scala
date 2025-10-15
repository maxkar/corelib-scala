package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad
import text.Location
import text.LocationInfo
import text.LocationTracker

/**
 * Standard implementation of the many input-related typeclasses.
 */
opaque type BufferedJsonReader[M[_]] = BufferedJsonReader.Impl[M, ?]


object BufferedJsonReader {
  /** Implementation of the reader. */
  private[v3] final class Impl[M[_]: Monad, S: Reader.In[M]](buffer: Array[Char], stream: S) {
    /** Limit after which we do buffer compaction. */
    private val compactionLimit = buffer.length - Peek.MAX_LOOK_AHEAD_DISTANCE
    /** Tracker of the input location. */
    private val location: LocationTracker = new LocationTracker()

    /** Read offset in the stream. */
    private var offset: Int = 0
    /** Number of buffered characters. */
    private var size: Int = 0
    /** If EOF was reached or not. */
    private var streamEof: Boolean = false

    /** Implementation of the Peek interface. */
    private[BufferedJsonReader] def peekImpl(offset: Int): M[Int] = {
      if size > offset then return Monad.pure(buffer(this.offset + offset))
      if streamEof then return Monad.pure(-1)
      fillBuffer() >=|| peekImpl(offset)
    }


    private[BufferedJsonReader] def readImpl(into: Array[Char], offset: Int, length: Int): M[Int] = {
      if size > 0 then {
        val readCount = Math.min(size, length)
        System.arraycopy(buffer, this.offset, into, offset, readCount)
        consume(readCount)
        return Monad.pure(readCount)
      }
      if streamEof then return Monad.pure(-1)

      fillBuffer() >=|| readImpl(into, offset, length)
    }


    private[BufferedJsonReader] def skipImpl(count: Int): M[Unit] = {
      if isEof() then return Monad.pure(())

      val consumedCount = Math.min(size, count)
      consume(consumedCount)
      if consumedCount == count then return Monad.pure(())

      return fillBuffer() >=|| skipImpl(count)
    }


    private[BufferedJsonReader] def skipWhileImpl(predicate: Char => Boolean): M[Unit] = {
      if isEof() then return Monad.pure(())

      val consumedCount = countMatching(predicate)
      consume(consumedCount)
      if size > 0 then return Monad.pure(())

      return fillBuffer() >=|| skipWhileImpl(predicate)
    }


    private[BufferedJsonReader] def readWhileImpl(into: StringBuilder, predicate: Char => Boolean): M[Unit] = {
      if isEof() then return Monad.pure(())

      val consumedCount = countMatching(predicate)
      into.appendAll(buffer, offset, consumedCount)
      consume(consumedCount)
      if size > 0 then return Monad.pure(())

      return fillBuffer() >=|| readWhileImpl(into, predicate)
    }


    private[BufferedJsonReader] def getLocationImpl(): M[Location] =
      Monad.pure(location.location())


    /** Fills buffer with the new data. */
    private def fillBuffer(): M[Unit] = {
      /* "Compact" the buffer by moving all the remaining data into the buffer start. */
      if offset > compactionLimit then {
        System.arraycopy(buffer, offset, buffer, 0, size)
        offset = 0
      }

      /* Fill more data into the buffer. */
      val writeOffset = offset + size
      stream.read(buffer, writeOffset, buffer.length - writeOffset) >-> { readCount =>
        if readCount < 0 then streamEof = true else size += readCount
      }
    }


    /** Consumes "count" characters from the buffer. */
    private def consume(count: Int): Unit = {
      location.update(buffer, offset, offset + count)
      if count == size then {
        size = 0
        offset = 0
      } else {
        size -= count
        offset += count
      }
    }


    /** Counts number of characters matching the predicate. */
    private def countMatching(predicate: Char => Boolean): Int = {
      val end = offset + size
      var ptr = offset
      while ptr < end && predicate(buffer(ptr)) do
        ptr += 1

      ptr - offset
    }


    /** Checks if the stream was read completely. */
    private def isEof(): Boolean = size == 0 && streamEof
  }


  /** Creates a new buffered reader of the `stream`. */
  def apply[M[_]: Monad, S: Reader.In[M]](stream: S, bufferSize: Int = 2048): BufferedJsonReader[M] = {
    if bufferSize <= Peek.MAX_LOOK_AHEAD_DISTANCE then
      throw new IllegalArgumentException(s"Buffer size ${bufferSize} is not sufficient for lookAhead of ${Peek.MAX_LOOK_AHEAD_DISTANCE}")

    new Impl(new Array[Char](bufferSize), stream)
  }


  given reader[M[_]]: Reader[M, BufferedJsonReader[M]] with {
    extension (stream: BufferedJsonReader[M]) {
      override def read(into: Array[Char], offset: Int, length: Int): M[Int] =
        stream.readImpl(into, offset, length)
    }
  }


  given defaultStream[M[_]]: DefaultStream[M, BufferedJsonReader[M]] with {
    extension (stream: BufferedJsonReader[M]) {
      override def skip(count: Int): M[Unit] =
        stream.skipImpl(count)

      override def skipWhile(predicate: Char => Boolean): M[Unit] =
        stream.skipWhileImpl(predicate)

      override def readWhile(into: StringBuilder, predicate: Char => Boolean): M[Unit] =
        stream.readWhileImpl(into, predicate)

      override def peek(offset: Int): M[Int] = {
        if offset < 0 then
          throw new IllegalArgumentException(s"Offset ${offset} could not be negative")
        if offset >= Peek.MAX_LOOK_AHEAD_DISTANCE then
          throw new IllegalArgumentException(s"Offset ${offset} should be less than ${Peek.MAX_LOOK_AHEAD_DISTANCE}")
        stream.peekImpl(offset)
      }
    }
  }

  given locationInfo[M[_]]: LocationInfo[M, BufferedJsonReader[M]] with {
    override def getLocation(stream: BufferedJsonReader[M]): M[Location] =
      stream.getLocationImpl()
  }
}
