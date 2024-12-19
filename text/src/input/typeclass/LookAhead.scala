package io.github.maxkar
package text.input.typeclass

/** A source of characters that provides "look ahead" functionality. */
trait LookAhead[M[_]] {
  /**
   * Requests the look-ahead to prepare at least `limit` characters for
   * look-ahead and indicate how much would be available. The return value
   * indicates how many characters would actually be available for look-ahead.
   *
   * @param limit a desired number of characters to look at before consuming
   *   the input.
   * @return a value of characters available for look-ahead. A return value
   *   `N < limit` indicates that the end-of-stream would be reached after
   *   consuming `N` characters. A return value of `N >= limit` indicates that
   *   at least `limit` characters are available.
   */
  def requestLookAhead(limit: Int): M[Int]


  /**
   * Looks at the character at the (positive) `offset` from the current position.
   *
   * It is advised that consumers call `requestLookAhead` method before invoking
   * the `lookAhead` method(s) like:
   *
   * ```
   *   for {
   *     _ <- requestLookAhead(2)
   *     a <- lookAhead(0)
   *     b <- lookAhead(1)
   *   }
   * ```
   *
   * @return the actual character value of negative value if end-of-stream would
   *   be reached earlier.
   */
  def lookAhead(offset: Int): M[Int]


  /**
   * Reads the content of the stream into the provided target buffer.
   * This methods fills the whole requested area unless end-of-stream was reached.
   *
   * @param target target to fill with data
   * @param targetStart starting offset in the buffer
   * @param targetEnd ending offset in the buffer
   * @return number `N` of filled character. If `N < targetEnd - targetStart` then
   *   end-of-stream was reached during the operation. A negative value is returned
   *   if no data was filled (and end-of-file was reached).
   */
  def read(target: Array[Char], targetStart: Int, targetEnd: Int): M[Int]


  /** Skips next `count` characters from the input. */
  def skip(count: Int): M[Unit]


  /**
   * Reads the characters matching the predicate into the provided buffer.
   * The return value is as follows:
   *   * A negative value N indicates that end-of-stream was reached an no
   *     data was filled
   *   * A value `N < targetEnd - targetStart` indicates that `N` characters
   *     was filled and then either end-of-stream was reached or predicate
   *     was not satisfied.
   *   * A value `N = targetEnd - targetStart` indicates that the whole buffer
   *     was filled.
   *
   * @param target target to fill with data
   * @param targetStart starting offset in the buffer
   * @param targetEnd ending offset in the buffer
   * @return number `N` of characters read or negative value if end-of-stream
   *   was reached.
   */
  def readWhile(
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        predicate: Char => Boolean
      ): M[Int]


  /** Skips all the characters satisfying the given predicate. */
  def skipWhile(predicate: Char => Boolean): M[Unit]
}


object LookAhead {
  inline def requestLookAhead[M[_]](limit: Int)(using la: LookAhead[M]): M[Int] =
    la.requestLookAhead(limit)


  inline def lookAhead[M[_]](offset: Int)(using la: LookAhead[M]): M[Int] =
    la.lookAhead(offset)


  inline def read[M[_]](
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int
      )(using
        la: LookAhead[M]
      ): M[Int] =
    la.read(target, targetStart, targetEnd)


  inline def skip[M[_]](count: Int)(using la: LookAhead[M]): M[Unit] =
    la.skip(count)


  inline def readWhile[M[_]](
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int,
        predicate: Char => Boolean
      )(using
        la: LookAhead[M]
      ): M[Int] =
    la.readWhile(target, targetStart, targetEnd, predicate)


  inline def skipWhile[M[_]](predicate: Char => Boolean)(using la: LookAhead[M]): M[Unit] =
    la.skipWhile(predicate)
}
