package io.github.maxkar
package text.input

import fun.typeclass.Monad

import java.nio.CharBuffer
import java.io.Reader
import java.io.IOException


/**
 * Implementation of the buffer manager and look-ahead stream.
 * @param filler data reader (similar to `java.io.Reader`).
 * @param buffer internal buffer containing intermediate data.
 */
private final class BufferLookAheadStream[M[_]: Monad](
      filler: BufferLookAheadStream.Filler[M],
      buffer: CharBuffer,
    )
    extends LookAheadStream[M] {

  /**
   * Amount of characters that were read. Read operations don't move data but
   * just update this pointer (client then has access to the chars).
   * Consequent look-ahead operations use this to properly adjust stream position.
   */
  private var skipCount: Int = -1

  /** If we observed EOF or not. */
  private var eofObserved: Boolean = false


  override def consume(count: Int): M[CharSequence] = {
    setConsumed(count)
    Monad.pure(buffer.subSequence(0, count))
  }


  override def skip(count: Int): M[Unit] = {
    setConsumed(count)
    Monad.pure(())
  }


  override def peek(minLength: Int): M[CharSequence] = {
    releaseResult()

    /* Happy path - everything is in memory or we know we can't add any more data. */
    if minLength <= buffer.remaining() || eofObserved then
      return Monad.pure(buffer)


    if minLength > buffer.capacity() then
      throw new IllegalArgumentException(
        s"Can't look at ${minLength} characters as this exceeds maximal limit of ${buffer.capacity()}"
      )

    val charsToRead = minLength - buffer.remaining()

    /* Check if we have to compact (which involves data copying) or could
     * reuse the existintg "tail" of the buffer.
     */
    if charsToRead > buffer.capacity() - buffer.limit() then
      buffer.compact()
      fillWithCharsAndReturn(charsToRead) {
        buffer.flip()
      }
    else {
      val start = buffer.position()
      /* First, set-up read position.
       * BEFORE:  start=position, <data>, limit,    capacity
       * AFTER :  start         , <data>, position, limit=capacity
       */
      buffer.position(buffer.limit())
      buffer.limit(buffer.capacity())
      fillWithCharsAndReturn(charsToRead) {
        /* Now we have new state. Return read pointers back.
         * BEFORE :  start         , <data>, <new-data>, position, limit=capacity
         * AFTER  :  position=start, <data>, <new-data>, limit,    capacity
         */
        buffer.limit(buffer.position())
        buffer.position(start)
      }
    }
  }


  override def releaseCharSequence(): M[Unit] = {
    releaseResult()
    Monad.pure(())
  }


  /** Sets the number of characters as "consumed" but does not adjust the stream. */
  private def setConsumed(count: Int): Unit = {
    if skipCount > 0 then
      throw new IllegalStateException(
        "Can't consume/skip two times in a row, look-ahead should be invoked between"
      )
    skipCount = count
  }


  /** Releases data that was returned before and returns a buffer portion for reuse. */
  private def releaseResult(): Unit = {
    if skipCount <= 0 then return
    buffer.position(buffer.position() + skipCount)
    skipCount = -1
  }


  /**
   * Fills the buffer with data, invokes the completion callback and then returs the buffer.
   */
  private def fillWithCharsAndReturn(charsToRead: Int)(onComplete: => Unit): M[CharSequence] = {
    val start = buffer.position()
    filler.fill(buffer, charsToRead).map { _ =>
      val read = buffer.position() - start
      /* Mark EOF to reduce calls and potential buffer compaction later. */
      if read < charsToRead then
        eofObserved = true
      /* Invoke handler to restore buffer context (proper read position and limit). */
      onComplete
      buffer
    }
  }
}


/**
 * CharBuffer-based implementation of the look-ahead stream.
 */
object BufferLookAheadStream {
  /**
   * Something that implements actual data reading/population. This trait is
   * slimmed-down and monad-aware version of the `java.io.Reader` interface.
   */
  trait Filler[M[_]] {
    /**
     * Fills the buffer with the data and guaranteeing that at least `minCharsToRead`
     * where read from the stream where possible. The implentation should only populate
     * the data and should not change `limit` (or move `position` backwards).
     *
     * The `minCharsToRead` advises on how much data the consumer needs to proceed
     * (the requested look-ahead).
     *
     * @param buffer buffer to fill. The buffer's position and capacity are set to the
     *   actual area to fill.
     * @param minCharsToRead minimal amount of characters to read. It is guaranteed
     *   that `minCharsToRead <= buffer.remaining()`. The implementation must read at least
     *   `minCharsToRead` as long as there is data. In other words, the implementation may
     *   read less than `minCharsToRead` only if end-of-stream was reached.
     */
    def fill(buffer: CharBuffer, minCharsToRead: Int): M[Unit]
  }


  object Filler {
    /**
     * Creates a new filler based on the `java.io.Reader`.
     * @param reader reader to use for accessing the data.
     * @param success encoding of the successfull operation.
     * @param failure encoder for IOexceptions raised during the operation.
     */
    def apply[M[_]](
          reader: Reader,
          success: M[Unit],
          failure: IOException => M[Unit],
        ): Filler[M] =
      new Filler[M] {
        override def fill(buffer: CharBuffer, minCharsToRead: Int): M[Unit] = {
          var toRead = minCharsToRead
          try {
            while toRead > 0 do {
              var rdCount = reader.read(buffer)
              if rdCount < 0 then
                return success
              else
                toRead -= rdCount
            }
            success
          } catch {
            case e: IOException => failure(e)
          }
        }
      }
  }


  /**
   * Creates a new buffered look-ahead stream. The maximal look ahead size
   * is same as buffer's capacity. However for performance reasons it is advised
   * that buffer's capacity is significantly (10-100 times) larger than the maximal
   * look-ahead size used during reading.
   * @param filler low-level input/output implementation.
   * @param buffer internal buffer.
   */
  def apply[M[_]: Monad](filler: Filler[M], buffer: CharBuffer): LookAheadStream[M] = {
    buffer.clear()
    buffer.limit(0)
    new BufferLookAheadStream(filler, buffer)
  }
}
