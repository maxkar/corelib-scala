package io.github.maxkar
package json.classic

import java.io.Writer

/** Serialization utilities. */
private[classic] object Serializer {
  /* Hexadecimal characters (for unicode encoding). */
  private val HEX_CHARS = "0123456789ABCDEF"


  /** Writes a json value. */
  def write(value: Json, stream: Writer): Unit =
    value match 
      case Json.Undefined => 
        throw new Json.Exception("Could not write undefined value")
      case Json.Null => 
        stream.write("null")
      case Json.True =>
        stream.write("true")
      case Json.False =>
        stream.write("false")
      case Json.Number(repr) =>
        stream.write(repr)
      case Json.String(x) =>
        writeString(x, stream)

      case Json.Array(items) =>
        stream.write("[")
        val itr = items.iterator
        if itr.hasNext then
          write(itr.next, stream)
        while itr.hasNext do
          stream.write(",")
          write(itr.next, stream)
        stream.write("]")

      case Json.Object(itemMap) =>
        stream.write("{")
        val itr = itemMap.iterator
        if itr.hasNext then
          writeEntry(itr.next, stream)
        while itr.hasNext do
          stream.write(",")
          writeEntry(itr.next, stream)
        stream.write("}")

    end match
  end write
      

  /** Writes one object entry. */
  private def writeEntry(entry: (String, Json), stream: Writer): Unit =
    writeString(entry._1, stream)
    stream.write(":")
    write(entry._2, stream)


  /** Writes a string with proper character encoding. */
  private def writeString(v: String, stream: Writer): Unit = 
    stream.write('"')
    val len = v.length
    var ptr = 0
    while ptr < len do
      writeChar(stream, v.charAt(ptr))
      ptr += 1
    stream.write('"')
  end writeString


  /** Outputs one character escaping it if needed. */
  private def writeChar(stream: Writer, c: Char): Unit =
    c match 
      case '"' => stream.write("\\\"")
      case '\\' => stream.write("\\\\")
      case '\b' => stream.write("\\b")
      case '\f' => stream.write("\\f")
      case '\n' => stream.write("\\n")
      case '\r' => stream.write("\\r")
      case '\t' => stream.write("\\t")
      case x if x < 0x0010 =>
        stream.write("\\u000")
        stream.write(toHexDigit(x))
      case x if x < 0x0020 =>
        stream.write("\\u001")
        stream.write(toHexDigit(x - 0x0010))
      case regular => stream.write(regular)
    end match
  end writeChar


  /** Converts an integer value into a correspondig hexadecimal digit. */
  private inline def toHexDigit(v: Int): Char =
    HEX_CHARS.charAt(v)
}
