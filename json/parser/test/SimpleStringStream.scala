package io.github.maxkar
package json.parser

import fun.typeclass.Applicative

import text.input.LookAheadStream

/**
 * Simple input implementation that reads the provided `data` string.
 * @param data string with the data that will be read by this "stream".
 * @param lookAheadSizeHint the "minimal size" hint for the buffer returned from
 *   `peek` calls. At least `sizeHint` characters is returned if present in
 *   the unconsumed part of the string.
 */
final class SimpleStringStream[M[_]: Applicative](
      data: String,
      lookAheadSizeHint: Int
    ) extends LookAheadStream[M] {
  /** Read pointer. */
  private var ptr = 0

  /** The furtherst look-ahead position. */
  private var lookAheadPtr = -1


  /**
   * Current read position in the stream.
   */
  def readOffset: Int = ptr


  override def peek(minLength: Int): M[CharSequence] = {
    if minLength <= 0 then
      throw new IllegalArgumentException(s"Peek's argument must be positive but is ${minLength}")

    val charsToRead = Math.min(data.length() - ptr, Math.max(minLength, lookAheadSizeHint))
    lookAheadPtr = ptr + charsToRead

    Applicative.pure(data.subSequence(ptr, lookAheadPtr))
  }


  override def consume(count: Int): M[CharSequence] = {
    if lookAheadPtr < 0 then
      throw new IllegalStateException("Could not consume with no look-ahead called")

    val newPtr = ptr + count
    if newPtr > lookAheadPtr then
      throw new IllegalArgumentException(
        s"Could not consume ${count} characters, only ${lookAheadPtr - ptr} could be consumed"
      )
    val resSeq = data.subSequence(ptr, newPtr)
    ptr = newPtr
    lookAheadPtr = -1
    Applicative.pure(resSeq)
  }


  override def skip(count: Int): M[Unit] = {
    if lookAheadPtr < 0 then
      throw new IllegalStateException("Could not skip with no look-ahead called")

    val newPtr = ptr + count
    if newPtr > lookAheadPtr then
      throw new IllegalArgumentException(
        s"Could not skip ${count} characters, only ${lookAheadPtr - ptr} could be consumed"
      )
    ptr = newPtr
    lookAheadPtr = -1
    Applicative.pure(())
  }


  override def releaseCharSequence(): M[Unit] =
    Applicative.pure(())
}


object SimpleStringStream {
  /** Returns all possible streams with various look-ahead sizes. */
  def allLookAheadSizes[M[_]: Applicative](data: String): Iterable[SimpleStringStream[M]] =
    (1 until data.size).map(new SimpleStringStream(data, _))


  /** Invokes callback on various read patterns. */
  def forAllLookAheadSizes[M[_]: Applicative](data: String)(cb: SimpleStringStream[M] => Unit): Unit =
    allLookAheadSizes(data).foreach(cb)
}
