package io.github.maxkar
package json.writer

/** Writer of the string value. */
private class StringOutputIterator(value: CharSequence) extends OutputIterator:
  import StringOutputIterator._
  import OutputIterator.NextResult

  /**
   * Current offset in the stream. Special values are:
   *  * -1 - before opening quote.
   *  * -2 - after the closing quote was written.
   */
  private var position = -1

  /**
   * Next special encoding (escaped character, etc...) to return.
   * The "special character" is detected during the regular scan over the sequence
   * (where a "regular" subsequence is detected) and the terminator is immediately stored
   * to avoid second round of parsing.
   */
  private var special: NextResult = null


  override def next(): NextResult =
    if position == -1 then
      position = 0
      return QUOTE

    if position == value.length() then
      position = -2
      return QUOTE

    if special != null then
      return getSpecial()

    if position < -1 then
      throw new IllegalStateException("Calling next when hasNext is false")

    val start = position
    scanRegularChars()

    if start < position then
      return NextResult.Result(value.subSequence(start, position))
    else
      return special
  end next


  /** Takes the "special" return value that represents escaped character inside the string. */
  private def getSpecial(): NextResult =
    val ret = special
    special = null
    position += 1
    return ret
  end getSpecial


  /**
   * Scans over the regular characters in the string (increments position) until it encounters
   * a special character or end of the stream.
   *
   * This method is guaranteed to do at least one of the following:
   *  * Advance the `pointer`.
   *  * Set the `special` value.
   */
  def scanRegularChars(): Unit =
    while position < value.length() do
      value.charAt(position) match
        case '"' =>
          special = ESC_QUOTE
          return
        case '\\' =>
          special = ESC_RSLASH
          return
        case '\b' =>
          special = ESC_BELL
          return
        case '\f' =>
          special = ESC_FF
          return
        case '\n' =>
          special = ESC_LF
          return
        case '\r' =>
          special = ESC_CR
          return
        case '\t' =>
          special = ESC_TAB
          return
        case c if c < 0x0020 =>
          special = LOWER_UNICODE(c)
          return
        case regular =>
          position += 1
      end match
    end while
  end scanRegularChars

end StringOutputIterator


private object StringOutputIterator:
  import OutputIterator.NextResult

  /** Quote character (outer quote for the string). */
  val QUOTE = OutputIterator.NextResult.Result("\"")

  /** Quote escape character. */
  val ESC_QUOTE = NextResult.Result("\\\"")

  /** Reverse slash escape character. */
  val ESC_RSLASH = NextResult.Result("\\\\")

  /** Bell (\b) escape character. */
  val ESC_BELL = NextResult.Result("\\b")

  /** Form feed escape character. */
  val ESC_FF = NextResult.Result("\\f")

  /** Carriage return escape character. */
  val ESC_CR = NextResult.Result("\\r")

  /** Line feed escape character. */
  val ESC_LF = NextResult.Result("\\n")

  /** Tab escape character. */
  val ESC_TAB = NextResult.Result("\\t")

  /** Hex digits. */
  private val HEX_DIGITS = "0123456789ABCDEF"


  /** Lower unicode range. JSON requires these to be always escaped.  */
  val LOWER_UNICODE: Array[OutputIterator.NextResult] =
    (
      HEX_DIGITS.map(v => OutputIterator.NextResult.Result("000${v}")) ++
      HEX_DIGITS.map(v => OutputIterator.NextResult.Result("001${v}"))
    ).toArray


end StringOutputIterator
