package io.github.maxkar
package text.input

import java.nio.CharBuffer

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
