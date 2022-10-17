package io.github.maxkar
package text.input

import fun.Applicative

import java.io.Reader
import java.io.IOException
import java.nio.CharBuffer

/**
 * Look ahead stream based on the underlying `java.io.Reader` implementation.
 * @tparam M execution monad.
 * @param peer reader for using in underlying operations.
 * @param buffer buffer for storing temporary data in memory.
 * @param ret function used to "return" value.
 * @param fail function used to encode failures into the monad.
 */
private final class ReaderLookAheadStream[M[_]](
      peer: Reader,
      buffer: CharBuffer,
      ret: [T] => T => M[T],
      fail: [T] => IOException => M[T]
    )
    extends LookAheadStream[M]:

  /**
   * Amount of characters that were read. Read operations don't move data but
   * just update this pointer. The following look-ahead operations use this to
   * properly adjust stream position.
   */
  private var skipCount: Int = -1

  /** Indicates that we observed End-Of-File condition.  */
  private var eofSeen: Boolean = false


  override def consume(count: Int): M[CharSequence] =
    if skipCount > 0 then
      throw new IllegalStateException(
        "Can't consume/skip two times in a row, look-ahead should be invoked inbetween"
      )
    skipCount = count
    ret(buffer.subSequence(0, count))
  end consume


  override def peek(minLength: Int): M[CharSequence] =
    updatePosition()

    /* Happy path - enough data or we know there won't be any new data. */
    if buffer.remaining() >= minLength || eofSeen then
      return ret(buffer)

    /* Now we have to fill the buffer. But first, let's check if the buffer is sufficient. */
    if minLength > buffer.capacity() then
      return fail(new IOException(
        s"Can't look ahead as the requested look-ahead of ${minLength} characters is larger than supported ${buffer.capacity()} characters."
      ))


    val charsToRead = minLength - buffer.remaining()

    try
      /* Prefer to not do compact as look ahead is expected to be significantly
       * less than capactiy so re-using space is better than constant compaction
       * (data copying).
       */
      if charsToRead <= buffer.capacity() - buffer.limit() then
        val start = buffer.position()
        /* First, set-up read position.
         * BEFORE:  start=position, <data>, limit,    capacity
         * AFTER :  start         , <data>, position, limit=capacity
         */
        buffer.position(buffer.limit())
        buffer.limit(buffer.capacity())
        fillWithChars(charsToRead)
        /* Now we have new state. Return read pointers back.
         * BEFORE :  start         , <data>, <new-data>, position, limit=capacity
         * AFTER  :  position=start, <data>, <new-data>, limit,    capacity
         */
        buffer.limit(buffer.position)
        buffer.position(start)
      else
        /* The "standard" way - compact, fill, flip. */
        buffer.compact()
        fillWithChars(charsToRead)
        buffer.flip()
      end if
    catch
      case e: IOException => return fail(e)
    end try

    ret(buffer)
  end peek


  override def skip(count: Int): M[Unit] =
    if skipCount > 0 then
      throw new IllegalStateException(
        "Can't consume/skip two times in a row, look-ahead should be invoked inbetween"
      )
    skipCount = count
    ret(())
  end skip


  override def releaseCharSequence(): M[Unit] =
    updatePosition()
    ret(())
  end releaseCharSequence


  /**
   * Updates buffer position by "skipping" the read data and making the buffer
   * space available for buffering next portion of data.
   */
  private def updatePosition(): Unit =
    if skipCount <= 0 then
      return
    buffer.position(buffer.position() + skipCount)
    skipCount = -1
  end updatePosition


  /**
   * Fills the buffer with at least `charsToRead` characters (or
   * end-of-file is reached).
   */
  private def fillWithChars(request: Int): Unit =
    var remaining = request

    while remaining > 0 && !eofSeen do
      val rd = peer.read(buffer)
      if rd < 0 then
        eofSeen = true
      else
        remaining -= rd
    end while
  end fillWithChars


end ReaderLookAheadStream
