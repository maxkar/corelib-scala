package io.github.maxkar
package json.writer

/** Writer of the string value. */
private class StringWriter(value: CharSequence) extends Iterator[CharSequence]:
  import StringWriter._

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
  private var special: String = null


  def hasNext: Boolean =
    position < -1


  override def next(): CharSequence =
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
      return value.subSequence(start, position)

    return special
  end next


  /** Takes the "special" return value that represents escaped character inside the string. */
  private def getSpecial(): CharSequence =
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
          special = "\\\""
          return
        case '\\' =>
          special = "\\\\"
          return
        case '\b' =>
          special = "\\b"
          return
        case '\f' =>
          special = "\\f"
          return
        case '\n' =>
          special = "\\n"
          return
        case '\r' =>
          special = "\\r"
          return
        case '\t' =>
          special = "\\t"
          return
        case c if c < 0x0020 =>
          special = LOWER_UNICODE(c)
          return
        case regular =>
          position += 1
      end match
    end while
  end scanRegularChars

end StringWriter


private object StringWriter:

  /** Quote character (outer quote for the string). */
  val QUOTE = "\""

  /** Hex digits. */
  private val HEX_DIGITS = "0123456789ABCDEF"


  /** Lower unicode range. JSON requires these to be always escaped.  */
  val LOWER_UNICODE: Array[String] =
    (
      HEX_DIGITS.map(v => s"000${v}") ++
      HEX_DIGITS.map(v => s"001${v}")
    ).toArray


end StringWriter
