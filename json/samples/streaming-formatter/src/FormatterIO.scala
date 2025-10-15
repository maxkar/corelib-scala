package io.github.maxkar
package json.sample.formatter.streaming

import fun.typeclass.Monad
import fun.instances.Unnest

import text.Location
import text.LocationTracker

import json.parser.Peek
import json.parser.ParseError
import json.parser.SkipStream
import json.writer.CharWriter
import json.writer.DefaultWriter

import java.io.Reader
import java.io.Writer
import java.io.IOException

/** Input/output operations for the formatter. */
final class FormatterIO(input: Reader, output: Writer) {
  /** Buffer of the unprocessed characters. */
  private val buffer: Array[Char] = new Array(4096)

  /** Read offset in the buffer. */
  private var offset: Int = 0

  /** Number of available characters. */
  private var size: Int = 0

  /** If end-of-file was observed in the stream. */
  private var eofObserved: Boolean = false

  /** Tracker of the current location. */
  private var locationTracker: LocationTracker = new LocationTracker()


  /** Copies the specified number of characters from one stream to another. */
  def copy(count: Int): Unit = {
    var remaining = count

    while remaining > 0 do {
      if size > 0 then {
        val copyCount = Math.min(remaining, size)
        copyPortion(copyCount)
        remaining -= copyCount
      } else if eofObserved then {
        throw new IOException(s"Unexpected eof, expected at least ${remaining} characters")
      } else {
        offset = 0
        readPortion()
      }
    }
  }


  /** Copies data while the predicate is satisfied. */
  def copyWhile(predicate: Char => Boolean): Unit = {
    while true do {
      if size > 0 then {
        var copyCount = 0
        while copyCount < size && predicate(buffer(offset + copyCount)) do
          copyCount += 1

        if copyCount == 0 then return
        copyPortion(copyCount)
      } else if eofObserved then {
        return
      } else {
        offset = 0
        readPortion()
      }
    }
  }


  /** Drops the given number of characters. */
  def drop(count: Int): Unit = {
    var remaining = count
    while remaining > 0 do {
      if size > 0 then {
        val dropCount = Math.min(size, remaining)
        dropPortion(dropCount)
        remaining -= dropCount
      } else if eofObserved then {
        throw new IOException(s"Unexpected eof, expected at least ${remaining} characters")
      } else {
        offset = 0
        readPortion()
      }
    }
  }


  /** Drops data while the predicate is satisfied. */
  def dropWhile(predicate: Char => Boolean): Unit = {
    while true do {
      if size > 0 then {
        var dropCount = 0
        while dropCount < size && predicate(buffer(offset + dropCount)) do
          dropCount += 1

        if dropCount == 0 then return
        dropPortion(dropCount)
      } else if eofObserved then {
        return
      } else {
        offset = 0
        readPortion()
      }
    }
  }


  /** Returns current location. */
  def getLocation(): Location = locationTracker.location()

  /** Checks if the stream is at EOF (i.e. was fully read). */
  def isEof(): Boolean = {
    if size > 0 then return false
    if eofObserved then return true

    readPortion()
    return size == 0
  }


  /** Implementation of the peek. */
  private def peekImpl(peekOffset: Int): Int = {
    if peekOffset < size then return buffer(offset + peekOffset)
    if eofObserved then return -1

    if offset > 0 then {
      System.arraycopy(buffer, offset, buffer, 0, size)
      offset = 0
    }

    while !eofObserved && size <= peekOffset do
      readPortion()

    if size <= peekOffset then -1 else buffer(peekOffset)
  }


  /** Implementation of a simple character write. */
  private def writeImpl(chr: Char): Unit = {
    output.write(chr)
  }


  /** Reads a portion of data. */
  private def readPortion(): Unit = {
    val rd = input.read(buffer, size, buffer.length - size)
    if rd < 0 then eofObserved = true else size += rd
  }


  /** Copies a portion of the given size. */
  private def copyPortion(portionSize: Int): Unit = {
    output.write(buffer, offset, portionSize)
    dropPortion(portionSize)
  }


  /** Drops a portion of the given size. */
  private def dropPortion(portionSize: Int): Unit = {
    locationTracker.update(buffer, offset, offset + portionSize)
    offset += portionSize
    size -= portionSize
  }
}


object FormatterIO {
  given SkipStream[Unnest, FormatterIO] with {
    extension (stream: FormatterIO) {
      override def peek(offset: Int): Unnest[Int] =
        Monad.pure(stream.peekImpl(offset))
      override def skip(count: Int): Unnest[Unit] =
        Monad.pure(stream.drop(count))
      override def skipWhile(predicate: Char => Boolean): Unnest[Unit] =
        Monad.pure(stream.dropWhile(predicate))
    }
  }


  given DefaultWriter[Unnest, FormatterIO] with {
    extension (stream: FormatterIO) {
      override def write(chr: Char): Unnest[Unit] =
        Monad.pure(stream.writeImpl(chr))
      override def write(cs: CharSequence): Unnest[Unit] =
        write(cs, 0, cs.length())
      override def write(cs: CharSequence, start: Int, end: Int): Unnest[Unit] = {
        var ptr = start
        while ptr < end do {
          stream.writeImpl(cs.charAt(ptr))
          ptr += 1
        }
        Monad.pure(())
      }
    }
  }


  given ParseError[Unnest, FormatterIO] with {
    extension (stream: FormatterIO) {
      override def parseError[T](message: String): Unnest[T] =
        throw new java.io.IOException(s"${stream.getLocation()}: message")
    }
  }
}
