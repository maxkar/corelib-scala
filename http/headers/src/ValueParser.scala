package io.github.maxkar
package http.headers

/**
 * Parser for the given header value.
 * @param data string to be parsed.
 */
class ValueParser(data: String):
  import ValueParser._

  /** Current offset in the `data`. */
  private[headers] var offset = 0

  /**
   * Extracts some context from header. Gets it around the offset. Used in generating
   * error messages.
   */
  private def getContext(): String =
    val start = Math.max(offset - 5, 0)
    val end = Math.min(data.length(), offset + 5)
    data.substring(start, end)
  end getContext


  /**
   * Raises an error. Does not echo the full header and only gives the brief context
   * so the (external) users have hard time exfiltrating the data (the headers may
   * be adjusted/mangled by the middleware).
   */
  private def raise[T](message: String): T =
    throw new HeaderFormatException(s"${offset}: ${message} (context: ${getContext()})")


  /**
   * Skips all (leading) whitespaces in the `data` stream.
   * Stops when next character is not whitespace or there is no more data.
   */
  def skipWhitespaces(): Unit =
    var ptr = offset
    while ptr < data.length() && isWhitespace(data.charAt(ptr)) do
      ptr += 1
    offset = ptr
  end skipWhitespaces


  /**
   * Checks if there is a first list element. Supports optional "empty" elements. Raises
   * an error if there are commas and whitespaces but no value.
   */
  def hasFirstListElement(): Boolean =
    var ptr = offset
    var hasComma = false
    while ptr < data.length() do
      data.charAt(ptr) match
        case ' ' | '\u0009' =>
          ptr += 1
        case ',' =>
          hasComma = true
          ptr += 1
        case _ =>
          offset = ptr
          return true
      end match
    end while

    offset = ptr
    if hasComma then
      raise("Header contains comma but no values")
    else
      false
  end hasFirstListElement


  /**
   * Checks if there is a next list element.
   */
  def hasNextListElement(): Boolean =
    var ptr = offset
    var hasComma = false
    while ptr < data.length() do
      data.charAt(ptr) match
        case ' ' | '\u0009' =>
          ptr += 1
        case ',' =>
          hasComma = true
          ptr += 1
        case _ =>
          /* We should ignore empty entries. "Empty" means we got at least one
           * comma (elemnt separator). There may be more than one comma (encoding
           * empty elements in the list)
           */
          if (hasComma)
            offset = ptr
            return true
          else
            offset = ptr
            raise("Expecting ',' before next list element")
      end match
    end while
    offset = ptr
    /* Fall-through. We are looking for "next" element so presumably list
     * is non-empty and we could ignore "empty" entries.
     */
    false
  end hasNextListElement


  /** Expects (and reads) a specific character. */
  def expectAndRead(c: Char): Unit =
    val ptr = offset
    if ptr >= data.length() then
      raise(s"Expecting '${c}' but got end of header")
    val hdrData = data.charAt(ptr)
    if hdrData != c then
      raise(s"Expecting '${c}' but got '${hdrData}'")
    offset = ptr + 1
  end expectAndRead


  /** Reads the token. */
  def readToken(): String =
    val start = offset
    var ptr = start

    while ptr < data.length() && isTokenChar(data.charAt(ptr)) do
      ptr += 1

    if ptr == start then
      raise("Token expected")
    offset = ptr
    data.substring(start, ptr)
  end readToken


  /** Reads a quoted string (the position should be at the opening quote char). */
  def readString(): String =
    var ptr = offset

    if ptr >= data.length() then
      raise("Expecting '\"' but got end of header")
    if data.charAt(ptr) != '"' then
      raise(s"Expecting '\"' but got '${data.charAt(ptr)}'")

    ptr += 1
    var sectionStart = ptr
    var agg: StringBuilder = null

    while ptr < data.length() do
      data.charAt(ptr) match
        case '"' =>
          offset = ptr + 1
          if agg == null then
            return data.substring(sectionStart, ptr)
          agg.append(data.subSequence(sectionStart, ptr))
          return agg.toString()
        case '\\' =>
          val sectionEnd = ptr

          ptr += 1
          if ptr >= data.length() then
            offset = ptr
            return raise("Expecting quoted char but got end of header")

          if agg == null then
            agg = new StringBuilder()
          agg.append(data.subSequence(sectionStart, sectionEnd))
          agg.append(data.charAt(ptr))

          ptr += 1
          sectionStart = ptr
        case other if isValidStringCharacter(other) =>
          ptr += 1
        case other =>
          offset = ptr
            return raise("Invalid string char")

          /* Has to deal with the quote, thus buffering. */
      end match
    end while

    offset = ptr
    raise("Expecting '\"' but got end of header")
  end readString


  /** Reads token or string, depending on what is in the stream. */
  def readTokenOrString(): String =
    var ptr = offset
    if ptr >= data.length() then
      raise("End of header when token or string is expected")
    if data.charAt(ptr) == '"' then
      readString()
    else
      readToken()
  end readTokenOrString


  /**
   * Reads parameters - set of key-value pairs.
   * This method returns keys and values as-is, the client will have to convert
   * parameters to the proper case for matching.
   */
  def readParameters(): Seq[(String, String)] =
    skipWhitespaces()

    if offset >= data.length() || data.charAt(offset) != ';' then
      return Seq.empty

    val buf = new scala.collection.mutable.ArrayBuffer[(String, String)]

    while offset < data.length() && data.charAt(offset) == ';' do
      offset += 1
      skipWhitespaces()
      val token = readToken()
      skipWhitespaces()
      expectAndRead('=')
      val value = readTokenOrString()
      buf += ((token, value))
      skipWhitespaces()
    end while

    buf.toSeq
  end readParameters


  /** Ensures the parser is at the end of the stream. */
  def expectEof(): Unit =
    skipWhitespaces()
    if offset < data.length() then
      raise("Extra data in the header")
  end expectEof

end ValueParser


/**
 * Parser for header values (or their common formats) and related functionality.
 * The formats and definitions are based on RFC 9110.
 */
object ValueParser:

  /** Checks if the character is treated as a delimiter in the HTTP specification. */
  def isDelimiter(char: Char): Boolean =
    char match
      case '"' | '(' | ')' | ',' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '{' | '}' => true
      case _ => false
  end isDelimiter


  /** Checks if the character is valid token element. */
  def isTokenChar(char: Char): Boolean =
    char >= 0x21 && char <= 0x7E && !isDelimiter(char)


  /** Checks if the character is valid whitespace according to the HTTP RFC. */
  def isWhitespace(char: Char): Boolean =
    char == ' ' || char == 0x09


  /** Checks if the character is valid string character (i.e. could occur inside the string). */
  def isValidStringCharacter(char: Char): Boolean =
    char match
      case '\t' => true  // explicitly allowed
      case '\\' | '\"' => false
      case x if 0x20 <= x && x <= 0x7E => true
      case x if 0x80 <= x && x <= 0xFF => true
      case other => false
    end match
  end isValidStringCharacter

  /** Checks if the character is valid escaped character (i.e. could occur inside the string escape). */
  def isValidEscapeCharacter(char: Char): Boolean =
    char match
      case '\t' | '\\' => true  // explicitly allowed
      case x if 0x20 <= x && x <= 0x7E => true
      case x if 0x80 <= x && x <= 0xFF => true
      case other => false
    end match
  end isValidEscapeCharacter
end ValueParser
