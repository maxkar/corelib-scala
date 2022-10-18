package io.github.maxkar
package text.input

import fun.Monad
import text.Location


/**
 * Look-ahead stream that is capable of tracking current input location.
 * @tparam M input monad.
 * @tparam A type of the (additional) context.
 * @param peer stream for delegating actual operations.
 * @param attachement additional context. Clients may use it to associate some
 *   useful data that is inherently associated with the input data and should be
 *   passed across the application. For example, an implementation may attach file
 *   name being read that is later used to provide error messages.
 */
final class LocationLookAheadStream[M[_]: Monad, +A] private(
      peer: LookAheadStream[M],
      val attachement: A,
    )
    extends LookAheadStream[M]:


  /** Source offset. */
  private var offset: Int = 0

  /** Current line. */
  private var line: Int = 1

  /** Curent column. */
  private var column: Int = 1

  /**
   * Indicates that the state is after carriage return (and thus a following line feed
   * should not increase line number.
   */
  private var afterCR: Boolean = false


  /**
   * Advances read position over the given sequence of input characters.
   *
   * The implementation uses "local look-ahead" to reduce amount of reads/writes to
   * the "afterCR" during iteration over the sequence.
   *
   * @param data data to advance over.
   * @return the original data sequence.
   */
  private def advance(data: CharSequence): CharSequence =
    val limit = data.length()

    if limit == 0 then
      return data

    var ptr = 0

    if afterCR then
      afterCR = false
      if data.charAt(ptr) == '\n' then
        ptr += 1
        offset += 1
      end if
    end if


    while ptr < limit do
      data.charAt(ptr) match
        case '\r' =>
          line += 1
          column = 1
          offset += 1

          val nextIdx = ptr + 1
          /* Keep the flag as the "read state", we need to carry this over. */
          if nextIdx >= limit then
            ptr = nextIdx
            afterCR = true
            return data
          end if

          if data.charAt(nextIdx) == '\n' then
            offset += 1
            ptr = nextIdx + 1
          else
            ptr = nextIdx
          end if

        case '\n' =>
          line += 1
          column = 1
          offset += 1
          ptr += 1

        case other =>
          offset += 1
          ptr += 1
          column += 1
      end match
    end while

    data
  end advance


  /** Current (read) position in the text. */
  def location: Location =
    Location(offset = offset, line = line, column = column)


  override def consume(count: Int): M[CharSequence] =
    peer.consume(count).map(advance)

  override def peek(minLength: Int): M[CharSequence] =
    peer.peek(minLength)

  override def skip(count: Int): M[Unit] =
    for
      data <- peer.consume(count)
      _ = advance(data)
      _ <- peer.releaseCharSequence()
    yield ()
  end skip

  override def releaseCharSequence(): M[Unit] =
    peer.releaseCharSequence()

end LocationLookAheadStream


object LocationLookAheadStream:
  /**
   * Creates a new look-ahead stream that also tracks current text location.
   * @param peer underlying data stream.
   * @param attachment additional data (like file name) that is associated with the stream
   *   and may be used by other stream consumers (for example, error handler). It is available
   *   as the `attachment` field on the returned object.
   */
  def apply[M[_]: Monad, A](
        peer: LookAheadStream[M],
        attachment: A = (),
      ): LocationLookAheadStream[M, A] =
    new LocationLookAheadStream(peer, attachment)
end LocationLookAheadStream
