package io.github.maxkar
package text.input

import fun.typeclass.Monad
import fun.typeclass.Applicative

/**
 * Stream of characters with look-ahead support.
 *
 * The methods could be divided into 3 groups: look-ahead, consumption and resource management.
 *
 * ## Look Ahead
 *
 * Look-ahead methods are `peekChar` and `peek`. They return some insight into
 * what is coming in in the stream. These methods should be called before any consumption
 * method is invoked.  The returned CharSequence should be valid until any other method is called
 * on this stream. Multiple look-ahead methods may be called in row.
 *
 * ## Consumption
 *
 * The methods `skip` and `consume` advance read pointer and (optionally) return the data
 * read. These methods should only be used after the caller invoked one of the *look-ahead*
 * methods and is sure that the `count` characters are exactly what it needs. Character
 * sequence returned from the stream should be valid until any other method is called on
 * this stream.
 *
 * Clients should not call more than one consumption methods without calling look-ahead
 * method(s) inbetween.
 *
 * ## Resource Management
 *
 * The only resource management method is `releaseCharSequence`. It is commonly used as a
 * final method during the parsing, after all the returned data is processed and indicates
 * that the (internal) character buffer may be used for other purpose(s).
 *
 * @tparam M type of the operation on the stream. I.e. synchronous, asynchronous, etc...
 */
trait LookAheadStream[M[_]]:
  /**
   * Attempts to look at the next `minLength` characters from the input stream. The
   * call may return less that `minLength` characters only if these are the very
   * last characters before end of file. Otherwise it should return at least `minLength`
   * characters (implementation are encouraged to return all characters available without
   * additional blocking and calls).
   *
   * @param minLength minimal length of the sequence to look at.
   * @return operation that yields a sequence of length of at least `minLength` size.
   */
  def peek(minLength: Int): M[CharSequence]


  /**
   * Skips the `count` following characters from the input stream.
   *
   * The call should always  follow a call to at least one of the **peek** methods
   * and `count` value should not  be greater than the length of the returned
   * sequence (1 for peekChar).
   */
  def skip(count: Int): M[Unit]


  /**
   * "Consumes" part of the input stream and returns the consumed data as a char sequence.
   *
   * The call should always  follow a call to at least one of the **peek** methods
   * and `count` value should not  be greater than the length of the returned
   * sequence (1 for peekChar).
   *
   * @return character sequence consisting of the next `count` characters from the stream.
   */
  def consume(count: Int): M[CharSequence]


  /**
   * Marks the latest returned character sequence as "released". Memory buffers
   * (if any) backing the sequence may now be reused for other purposes.
   */
  def releaseCharSequence(): M[Unit]
end LookAheadStream


/**
 * Utilities and factories for look-ahead streams.
 */
object LookAheadStream:
  /**
   * Creates a new look-ahead stream that also tracks current text location.
   * @param peer underlying data stream.
   * @param attachment additional data (like file name) that is associated with the stream
   *   and may be used by other stream consumers (for example, error handler). It is available
   *   as the `attachment` field on the returned object.
   */
  def trackLocation[M[_]: Monad, A](
        peer: LookAheadStream[M],
        attachment: A = (),
      ): LocationLookAheadStream[M, A] =
    LocationLookAheadStream(peer, attachment)


  /**
   * Creates a look-ahead stream that just reads the provided character sequence.
   */
  def fromCharSequence[M[_]: Applicative](data: CharSequence): LookAheadStream[M] =
    new SimpleLookAheadStream(data)
end LookAheadStream
