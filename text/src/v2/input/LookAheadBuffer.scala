package io.github.maxkar
package text.v2.input

import text.Location
import text.LocationTracker


/**
 * Buffer for look-ahead operations.
 * @param buffer buffer for reading.
 */
private final class LookAheadBuffer(buffer: Array[Char]) {
  /** Indicates if end-of-file was observed. */
  private var eof = false

  /** Position where we should start reading the buffer. */
  private var readOffset: Int = 0

  /** Position where we should start writing into the buffer. */
  private var writeOffset: Int = 0

  /** Size of the buffer (number of characters to read). */
  var size: Int = 0

  /** Tracker of the input location. */
  private val locationTracker: LocationTracker = new LocationTracker

  /** If buffer is wrapped around and is not continuous. */
  private def wrapAround: Boolean = buffer.length - size <= readOffset

  /** Returns the (total) capacity of the buffer. */
  def capacity: Int = buffer.length

  /** Checks if the buffer is marked as "at the end of file". */
  def isEof: Boolean = eof

  /** Marks this buffer as "having end of file observed". */
  def markEof(): Unit = eof = true

  /** Buffer where the new data should be put. */
  def writeBuffer: Array[Char] = buffer

  /** Offset where the buffer should start. */
  def writeStart: Int = writeOffset

  /** Offset where write to the buffer should end. */
  def writeEnd: Int = if wrapAround then readOffset else buffer.length

  /** Offset where we can start reading. */
  private def readStart: Int = readOffset

  /** Offset where we can end reading. */
  private def readEnd: Int = if wrapAround then buffer.length else writeStart + size

  /** Updates buffer state after some characters were written. */
  def dataWritten(count: Int): Unit = {
    writeOffset += count
    if writeOffset == buffer.length then
      writeOffset = 0
    size += count
  }

  /** Returns current location in the text. */
  def location: Location = locationTracker.location()

  /** Looks at the character from the given input offset. */
  def lookAhead(count: Int): Int =
    if buffer.length - count <= readOffset then
      buffer(readOffset + count - buffer.length)
    else
      buffer(readOffset + count)


  /** Skips the specified number of characters. */
  def skip(count: Int): Unit = {
    var remaining = count
    if buffer.length - count <= readOffset then {
      locationTracker.update(buffer, readOffset, buffer.length)
      readOffset = 0
      remaining -= buffer.length - readOffset
    }

    locationTracker.update(buffer, readOffset, readOffset + remaining)
    readOffset += remaining
    size -= count
  }


  /** Reads as much data as possible into the buffer. */
  def read(target: Array[Char], targetStart: Int, targetEnd: Int): Int = {
    val readCount = Math.min(size, targetEnd - targetStart)
    var targetPos = targetStart
    var readRemaining = readCount

    if buffer.length - readCount <= readOffset then {
      val batchSize = buffer.length - readOffset
      System.arraycopy(buffer, readOffset, target, targetStart, batchSize)
      locationTracker.update(buffer, readOffset, readOffset + batchSize)
      readOffset = 0
      targetPos += batchSize
      readRemaining -= batchSize
    }

    System.arraycopy(buffer, readOffset, target, targetPos, readRemaining)
    locationTracker.update(buffer, readOffset, readOffset + readRemaining)
    readOffset += readRemaining

    size -= readCount

    readCount
  }


  /** Skips data while predicate is satisfied. */
  def skipWhile(predicate: Char => Boolean): Int = {
    var readPtr = readOffset
    var readRemaining = size

    while readRemaining > 0 && predicate(buffer(readPtr)) do {
      locationTracker.update(buffer(readPtr))
      readRemaining -= 1
      readPtr += 1
      if readPtr == buffer.length then
        readPtr = 0
    }

    val readCount = size - readRemaining
    readOffset = readPtr
    size = readRemaining
    readCount
  }


  /** Reads data while predicate is satisfied. */
  def readWhile(target: Array[Char], targetStart: Int, targetEnd: Int, predicate: Char => Boolean): Int = {
    val maxRead = Math.min(size, targetEnd - targetStart)
    var targetPtr = targetStart
    var readPtr = readOffset
    var readRemaining = maxRead

    while readRemaining > 0 && predicate(buffer(readPtr)) do {
      target(targetPtr) = buffer(readPtr)
      locationTracker.update(buffer(readPtr))
      targetPtr += 1
      readRemaining -= 1
      readPtr += 1
      if readPtr == buffer.length then
        readPtr = 0
    }

    val readCount = targetPtr - targetStart
    readOffset = readPtr
    size -= readCount
    readCount
  }
}


private object LookAheadBuffer {
  /** Creates a new look-ahead buffer of the requested capacity. */
  def apply(capacity: Int): LookAheadBuffer =
    new LookAheadBuffer(new Array[Char](capacity))
}


