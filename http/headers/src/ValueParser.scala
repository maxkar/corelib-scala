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
  private def raise(message: String): Either[String, Nothing] =
    Left(s"${offset}: ${message} (context: ${getContext()})")


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
  def hasFirstListElement(): Either[String, Boolean] =
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
          return Right(true)
      end match
    end while

    offset = ptr
    if hasComma then
      raise("Header contains comma but no values")
    else
      Right(false)
  end hasFirstListElement


  /**
   * Checks if there is a next list element.
   */
  def hasNextListElement(): Either[String, Boolean] =
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
            return Right(true)
          else
            offset = ptr
            return raise("Expecting ',' before next list element")
      end match
    end while
    offset = ptr
    /* Fall-through. We are looking for "next" element so presumably list
     * is non-empty and we could ignore "empty" entries.
     */
    Right(false)
  end hasNextListElement


  /** Expects (and reads) a specific character. */
  def expectAndRead(c: Char): Either[String, Unit] =
    val ptr = offset
    if ptr >= data.length() then
      return raise(s"Expecting '${c}' but got end of header")
    val hdrData = data.charAt(ptr)
    if hdrData != c then
      return raise(s"Expecting '${c}' but got '${hdrData}'")
    offset = ptr + 1
    Right(())
  end expectAndRead


  /** Reads the token. */
  def readToken(): Either[String, String] =
    val start = offset
    var ptr = start

    while ptr < data.length() && isTokenChar(data.charAt(ptr)) do
      ptr += 1

    if ptr == start then
      return raise("Token expected")
    offset = ptr
    Right(data.substring(start, ptr))
  end readToken


  /** Reads a quoted string (the position should be at the opening quote char). */
  def readString(): Either[String, String] =
    var ptr = offset

    if ptr >= data.length() then
      return raise("Expecting '\"' but got end of header")
    if data.charAt(ptr) != '"' then
      return raise(s"Expecting '\"' but got '${data.charAt(ptr)}'")

    ptr += 1
    var sectionStart = ptr
    var agg: StringBuilder = null

    while ptr < data.length() do
      data.charAt(ptr) match
        case '"' =>
          offset = ptr + 1
          if agg == null then
            return Right(data.substring(sectionStart, ptr))
          agg.append(data.subSequence(sectionStart, ptr))
          return Right(agg.toString())
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
  def readTokenOrString(): Either[String, String] =
    var ptr = offset
    if ptr >= data.length() then
      return raise("End of header when token or string is expected")
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
  def readParameters(): Either[String, Seq[(String, String)]] =
    skipWhitespaces()

    if offset >= data.length() || data.charAt(offset) != ';' then
      return Right(Seq.empty)

    val buf = new scala.collection.mutable.ArrayBuffer[(String, String)]

    while offset < data.length() && data.charAt(offset) == ';' do
      offset += 1
      skipWhitespaces()
      val token =
        readToken() match
          case Left(err) => return Left(err)
          case Right(v) => v
        end match
      skipWhitespaces()
      expectAndRead('=') match
        case Left(err) => return Left(err)
        case Right(_) => ()
      end match
      val value =
        readTokenOrString() match
          case Left(err) => return Left(err)
          case Right(v) => v
        end match
      buf += ((token, value))
      skipWhitespaces()
    end while

    Right(buf.toSeq)
  end readParameters
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
