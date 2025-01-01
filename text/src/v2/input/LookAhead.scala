package io.github.maxkar
package text.v2.input

import fun.typeclass.Applicative

import scala.annotation.targetName

/**
 * A support for look-ahead on streams with type `T` and using
 * `M` as the operation type.
 */
trait LookAhead[M[_], T] extends Reader[M, T] {
  /**
   * Fill the look-ahead buffer for the stream and returns a number
   * of characters available.
   * @param stream stream to fill.
   * @param request number of characters to pre-populate. This indicates a
   *   most likely number of characters that will be looked-at by the client.
   * @return a (non-negative) number of characters available for look-ahead.
   *   A returned value `N < request` indicates that the end of stream will
   *   be reached in the next `N` characters.
   */
  def fill(stream: T, request: Int): M[Int]


  /**
   * Peeks a character at the given offset from the current position.
   * @return a character at the given offset or a negative value if end-of-stream
   *   is reached before the `offset`.
   */
  def peek(stream: T, offset: Int): M[Int]


  /** Skips next `count` characters from the stream. */
  def skip(stream: T, count: Int): M[Unit]


  /**
   * Reads a requested number of characters from the stream. Unlike the general
   * reader, this call populates as much bytes as available in the stream.
   */
  override def read(stream: T, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int]


  /**
   * Skips characters while the predicate is satisfied.
   */
  def skipWhile(stream: T, predicate: Char => Boolean): M[Unit]


  /** Reads the characters and populates the buffer only while the predicate is satisfied. */
  def readWhile(
        stream: T,
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        predicate: Char => Boolean,
      ): M[Int]


  extension (t: T) {
    @targetName("fillExt")
    inline def fill(request: Int): M[Int] =
      this.fill(t, request)

    @targetName("peekExt")
    inline def peek(offset: Int): M[Int] =
      this.peek(t, offset)

    @targetName("skipExt")
    inline def skip(count: Int): M[Unit] =
      this.skip(t, count)

    @targetName("skipWhileExt")
    inline def skipWhile(predicate: Char => Boolean): M[Unit] =
      this.skipWhile(t, predicate)

    @targetName("readWhileExt")
    inline def readWhile(
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int,
          predicate: Char => Boolean
        ): M[Int] =
      this.readWhile(t, target, targetStart, targetEnd, predicate)
  }
}

object LookAhead {
  /** End-of-input implementation for look-ahead streams. */
  given endOfInput[M[_]: Applicative, S](using la: LookAhead[M, S]): EndOfInput[M, S] with {
    override def atEndOfInput(stream: S): M[Boolean] =
      stream.peek(0) <| { next => next < 0 }
  }
}
