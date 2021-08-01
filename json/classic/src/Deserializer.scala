package io.github.maxkar
package json.classic

import java.io.Reader
import java.io.ByteArrayInputStream
import java.io.InputStreamReader


/** Json reader/deserializer. */
private object Deserializer:

  /** 
   * Parses a byte array assuming it is using UTF-8 encoding (RFC 8259). 
   * @see {{{parseRFC7158}}} if you need automatic encoding sniffing.
   */
  def parse(bytes: Array[Byte]): Json =
    parseWithEncoding(bytes, "UTF-8")



  /** 
   * Parses a byte array automatically detecting JSON encoding.
   * The RFC 8259 makes UTF-8 the only supported format thus the default read method change.
   */
  def parseRFC7158(bytes: Array[Byte]): Json =
    parseWithEncoding(bytes, sniffEncoding(bytes))



  /** Parses a JSON value from the underlying stream. */
  def parse(rdr: Reader): Json = 
    val stream = JsonStream(rdr)
    val res = readValue(stream)

    skipWhites(stream)
    if !stream.eof() then
      throw Json.Exception("Trailing data after the end of json at " + stream.location())
    res
  end parse



  /** Parses array of bytes with the provided encoding. */
  private def parseWithEncoding(bytes: Array[Byte], enc: String): Json =
    parse(InputStreamReader(ByteArrayInputStream(bytes), enc))



  /** Reads a single value from the input stream. */
  private def readValue(stream: JsonStream): Json =
    skipWhites(stream)
    stream.peek() match
      case 't' => 
        skipLiteral(stream, "true")
        Json.True
      case 'f' =>
        skipLiteral(stream, "false")
        Json.False
      case 'n' =>
        skipLiteral(stream, "null")
        Json.Null
      case '"' => Json.String(readString(stream))
      case '{' => readJsonObject(stream)
      case '[' => readJsonArray(stream)
      case '-' => readNumber(stream)
      case x if '0' <= x && x <= '9' => readNumber(stream)
      case x if x < 0 =>
        throw Json.Exception("Unexpected end of file at " + stream.location() + " while looking for value")
      case x =>
        throw Json.Exception("Illegal start of json value at " + stream.location() + " char = " + x)
    end match
  end readValue



  /** 
   * Skips a single JSON literal from the input stream.
   * @param stream stream to skip.
   * @param repr representation of the literal to read.
   * @throws Json.Exception if the next characters does not match literal representation.
   */
  private def skipLiteral(stream: JsonStream, repr: String): Unit =
    val start = stream.location()
    val len = repr.length()
    
    var ptr = 0
    while ptr < len do
      if stream.eof() then
        throw Json.Exception(
          s"Unexpected EOF at ${stream.location()} while reading literal ${repr} started at ${start}"
        )
      
      val nxt = stream.read()
      if repr.charAt(ptr) != nxt then
        throw Json.Exception(
          s"Mailformed literal ${repr} at ${start}, expected ${repr.charAt(ptr)} at ${stream.location()} but got ${nxt}"
        )

      ptr += 1
    end while

  end skipLiteral



  /** Reads a string from the stream. */
  private def readString(stream: JsonStream): String =
    val start = stream.location()
    stream.read() // '"'

    val buf = StringBuilder()
    while stream.peek() != '"' do
      if stream.eof() then
        throw Json.Exception(
          s"Malformed string at ${start}, could not find closing quote till stream end at ${stream.location()}"
        )
      buf += readStringChar(stream)
    end while
    stream.read() // '"'

    buf.toString()
  end readString



  /** Reads one string character (unescaping if needed). */
  private def readStringChar(stream: JsonStream): Char =
    if stream.peek() != '\\' then
      if stream.peek() < 0x0020 then
        throw Json.Exception(
          s"Unescaped special character in string at ${stream.location()}"
        )
      return stream.read()
    end if
    
    stream.read() // '\\'

    val escapeLoc = stream.location()
    if stream.eof() then
      throw Json.Exception(s"EOF after escape at ${escapeLoc}")

    stream.read() match 
      case '"' => '"'
      case '\\' => '\\'
      case '/' => '/'
      case 'b' => '\b'
      case 'f' => '\f'
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case 'u' => readUnicodeChar(stream)
      case x => 
        throw Json.Exception(
          s"Unrecognizeable escaped character ${x} at ${escapeLoc}"
        )
    end match
  end readStringChar



  /** Reads a single unicode character. */
  private def readUnicodeChar(stream: JsonStream): Char =
    ((readUnicodeDigit(stream) << 12) |
     (readUnicodeDigit(stream) << 8) |
     (readUnicodeDigit(stream) << 4) |
      readUnicodeDigit(stream)
    ).toChar



  /** Reads a single unicode digit (hex digit as part of an unicode escape). */
  private def readUnicodeDigit(stream: JsonStream): Int =
    if stream.eof() then
      throw Json.Exception(
        s"Unexpected EOF inside unicode escape at ${stream.location()}"
      )

    stream.read() match 
      case n if '0' <= n && n <= '9' => n - '0'
      case n if 'A' <= n && n <= 'F' => n - 'A' + 10
      case n if 'a' <= n && n <= 'f' => n - 'a' + 10
      case other =>
        throw Json.Exception(s"Invalid unicode digit before ${stream.location()}")
    end match
  end readUnicodeDigit



  /** Reads a number from the stream. */
  private def readNumber(stream: JsonStream): Json =
    val res = StringBuilder()

    /* Sign. */
    if stream.peek() == '-' then
      res += stream.read()

    if stream.eof() then
      throw Json.Exception(
        s"Unexpected EOF while expecting a number at ${stream.location()}"
      )

    if stream.peek() == '0' then
      res += stream.read()
      if !stream.eof() && Character.isDigit(stream.peek()) then
        throw Json.Exception(
          s"Could not read a number with leading zero at ${stream.location()}"
        )
    else if Character.isDigit(stream.peek()) then
      while !stream.eof() && Character.isDigit(stream.peek()) do
        res += stream.read()
    else 
      throw Json.Exception(
        s"Illegal number start character ${stream.peek()} at ${stream.location()}"
      )

    /* Frac. */
    if stream.peek() == '.' then
      res += stream.read() // '.'
      if stream.eof() || !Character.isDigit(stream.peek()) then
        throw Json.Exception(s"Illegal fractional part at ${stream.location()}")
      
      while !stream.eof() && Character.isDigit(stream.peek()) do
        res += stream.read()
    end if /* Frac. */

    /* Exp. */
    if stream.peek() == 'e' || stream.peek() == 'E' then
      res += stream.read() // 'e' or 'E'

      if stream.peek() == '+' || stream.peek() == '-' then
        res += stream.read()
        
      if stream.eof() || !Character.isDigit(stream.peek()) then
        throw Json.Exception(s"Illegal exponent at ${stream.location()}")
      while !stream.eof() && Character.isDigit(stream.peek()) do
        res += stream.read()
    end if /* Exp. */

    Json.Number(res.toString())
  end readNumber



  /** Reads a JSON object value. */
  private def readJsonObject(stream: JsonStream): Json =
    val start = stream.location()
    stream.read() // '{'
    skipWhites(stream)

    if stream.peek() == '}' then
      stream.read()
      return Json.Object(Map.empty)

    val defMap = scala.collection.mutable.HashMap[String, (String, Json)]()
    defMap += readObjectEntry(stream)
    skipWhites(stream)

    /* Scan through entries. */
    while stream.peek() != '}' do
      if stream.peek() != ',' then
        throw Json.Exception(
          s"Malformed object at ${start}, got ${stream.peek()} at ${stream.location()} while looking for '}' or ','"
        )
      stream.read() // ','
      skipWhites(stream)

      val (key, (loc, value)) = readObjectEntry(stream)

      defMap.get(key) match
        case Some((prevLoc, _)) =>
          throw Json.Exception(
            s"Redifinition of ${key} at ${loc}, previously defined at ${prevLoc}"
          )
        case None => ()
      end match

      defMap += (key -> (loc, value))
      skipWhites(stream)
    end while /* Scan through entries. */

    stream.read() // '}'
    Json.Object(defMap.view.mapValues(_._2).toMap)
  end readJsonObject



  /** Reads one object entry (key-value pair). */
  private def readObjectEntry(stream: JsonStream): (String, (String, Json)) = 
    if stream.peek() != '"' then
      throw Json.Exception(
        s"Bad object entry start at ${stream.location()}, expected '\"' but got ${stream.peek()}"
      )

    val defStart = stream.location()
    val name = readString(stream)
    skipWhites(stream)

    if stream.peek() != ':' then
      throw Json.Exception(
        s"Expected : in the object body but got ${stream.peek()} at ${stream.location()}"
      )
    stream.read() // ':'

    (name, (defStart, readValue(stream)))
  end readObjectEntry



  /** Reads a json array from the stream. */
  private def readJsonArray(stream: JsonStream): Json = 
    val start = stream.location()

    stream.read() // '['
    skipWhites(stream)

    if stream.peek() == ']' then
      stream.read() // ']'
      return Json.Array(Seq.empty)
    
    val items = scala.collection.mutable.ArrayBuffer[Json]()
    items += readValue(stream)
    skipWhites(stream)

    while stream.peek() != ']' do
      if stream.peek() != ',' then
        throw Json.Exception(
          s"Malformed array at ${start}, got ${stream.peek()} while looking for ',' or ']' at ${stream.location()}"
        )
      
      stream.read() // ','
      items += readValue(stream)
      skipWhites(stream)
    end while
    stream.read() // ']'

    Json.Array(items.toSeq)
  end readJsonArray



  /** Skips all whitespaces from the stream. */
  private def skipWhites(stream: JsonStream): Unit =
    while isWhitespace(stream.peek()) do
      stream.read()



  /** Checks if the character is JSON whitespace. */
  private def isWhitespace(c: Int): Boolean = 
    c match 
      case ' ' | '\t' | '\r' | '\n' => true
      case _ => false



  /** 
   * Sniffs encoding according to RFC 4627, chapter 3 
   * (the same alternatives are supported by RFC 7159). 
   */
  private def sniffEncoding(bytes: Array[Byte]): String =
    if bytes.length == 2 then
      if bytes(0) == 0 then 
        return "UTF-16BE"
      else if bytes(1) == 0 then 
        return "UTF-16LE"
      else 
        return "UTF-8"

    /* 0, 1 or 3 bytes. */
    if (bytes.length < 4)
      return "UTF-8"

    if bytes(0) == 0 && bytes(1) == 0 && bytes(2) == 0 && bytes(3) != 0 then
      return "UTF-32BE"
    if bytes(0) == 0 && bytes(1) != 0 && bytes(2) == 0 && bytes(3) != 0 then
      return "UTF-16BE"
    if bytes(0) != 0 && bytes(1) == 0 && bytes(2) == 0 && bytes(3) == 0 then
      return "UTF-32LE"
    if bytes(0) != 0 && bytes(1) == 0 && bytes(2) != 0 && bytes(3) == 0 then
      return "UTF-16LE"
    if bytes(0) != 0 && bytes(1) != 0 && bytes(2) != 0 && bytes(3) != 0 then
      return "UTF-8"
    throw Json.Exception(s"Invalid byte start pattern ${bytes(0)},${bytes(1)},${bytes(2)},${bytes(3)}")
  end sniffEncoding
  
end Deserializer
