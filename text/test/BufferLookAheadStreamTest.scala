package io.github.maxkar
package text.input

import fun.typeclass.Monad
import java.nio.CharBuffer

/**
 * Tests for the buffered look-ahead stream tests.
 */
final class BufferLookAheadStreamTest extends org.scalatest.funsuite.AnyFunSuite:

  /** Identity type for simplifying operations. */
  type Identity[T] = T

  /** Implementation of the monad for the identity type. */
  given identityMonad: Monad[Identity] with
    override def pure[T](x: T): T = x

    override def bind[S, R](v: Identity[S], fn: S => Identity[R]): Identity[R] = fn(v)
  end identityMonad


  /**
   * Data feeder that has controllable chunks.
   * @param source data that is being fed into the stream.
   * @param nextFeedSize portion that will be returned on the next request to feed.
   */
  final class DynamicFeeder(
        source: String,
        var nextFeedSize: Int,
      )
      extends BufferLookAheadStream.Filler[Identity]:

    /** Current write pointer. */
    private var ptr = 0


    override def fill(buffer: CharBuffer, minCharsToRead: Int): Identity[Unit] =
      assert(minCharsToRead <= nextFeedSize)
      assert(buffer.remaining() >= nextFeedSize)
      assert(nextFeedSize <= source.length() - ptr)
      val end = ptr + nextFeedSize
      buffer.append(source.subSequence(ptr, end))
      ptr = end
    end fill
  end DynamicFeeder


  test("Wrap-around executed as expected") {
    val str = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val feeder = new DynamicFeeder(str, 3)
    val buf = CharBuffer.allocate(7)
    val buffered = BufferLookAheadStream(feeder, buf)

    assert(buffered.peek(2).toString() === str.subSequence(0, 3))
    assert(buffered.consume(1).toString() === str.subSequence(0, 1))

    assert(buffered.peek(2).toString() === str.subSequence(1, 3))
    assert(buffered.consume(2).toString() === str.subSequence(1, 3))
    /* Buffer -> pos=limit=3,capacity=7; Str -> pos=3 */

    assert(buffered.peek(1).toString() === str.subSequence(3, 6))
    assert(buffered.peek(3).toString() === str.subSequence(3, 6))
    assert(buffered.consume(3).toString() === str.subSequence(3, 6))
    /* Buffer -> pos=limit=6,capacity=7; Str -> pos=6 */

    feeder.nextFeedSize = 1
    assert(buffered.peek(1).toString() === str.subSequence(6, 7))
    /* Buffer -> pos=6,limit=7,capacity=7; Str -> pos=6 */

    /* Now, wrap-around + copying. */
    feeder.nextFeedSize = 2
    assert(buffered.peek(2).toString() === str.subSequence(6, 9))
    /* Buffer -> pos=0,limit=3,capacity=7; Str -> pos=6 */
    assert(buffered.consume(3).toString() === str.subSequence(6, 9))
    /* Buffer -> pos=limit=3,capacity=7; Str -> pos=9 */

    feeder.nextFeedSize = 3
    assert(buffered.peek(2).toString() === str.subSequence(9, 12))
    /* Buffer -> pos=3,limit=6,capacity=7; Str -> pos=9 */
    assert(buffered.peek(5).toString() === str.subSequence(9, 15))
    /* Buffer -> pos=0,limit=6,capacity=7; Str -> pos=9 */
    assert(buffered.consume(5).toString() === str.subSequence(9, 14))

    feeder.nextFeedSize = 1
    assert(buffered.peek(2).toString() === str.subSequence(14, 16))
    /* Buffer -> pos=5,limit=7,capacity=7; Str -> pos=14 */
  }
end BufferLookAheadStreamTest
