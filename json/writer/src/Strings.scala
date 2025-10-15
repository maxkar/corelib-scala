package io.github.maxkar
package json.writer

import fun.typeclass.Monad


/** Utilities for working with strings. */
object Strings {
  /** String boundary character. */
  val STRING_BOUNDARY = '"'

  /** Hex digits. */
  private val HEX_DIGITS = "0123456789ABCDEF"


  /** Writes one character into the stream. */
  def write[M[_]: Monad, S: CharWriter.In[M]](stream: S, chr: Char): M[Unit] =
    chr match {
      case '"' => writeSimpleEscape(stream, '"')
      case '\\' => writeSimpleEscape(stream, '\\')
      case '\b' => writeSimpleEscape(stream, 'b')
      case '\f' => writeSimpleEscape(stream, 'f')
      case '\r' => writeSimpleEscape(stream, 'r')
      case '\n' => writeSimpleEscape(stream, 'n')
      case '\t' => writeSimpleEscape(stream, 't')
      case x if x < 0x0020 => writeUnicodeEscape(stream, x)
      case _ => stream.write(chr)
    }


  /** Checks if the character is special and must be escaped. */
  def needsEscape(chr: Char): Boolean =
    chr match {
      case '"' | '\\' | '\b' | '\f' | '\r' | '\n' | '\t' => true
      case x if x < 0x0020 => true
      case _ => false
    }


  /** Writes a char sequence as a part of the string. */
  def writePart[M[_]: Monad, S: DefaultWriter.In[M]](stream: S, chars: CharSequence): M[Unit] =
    writePart(stream, chars, 0, chars.length())


  /** Writes a part of the char sequence as a part of the string. */
  def writePart[M[_]: Monad, S: DefaultWriter.In[M]](
        stream: S,
        chars: CharSequence,
        start: Int,
        finish: Int
      ): M[Unit] = {
    if start >= finish then return Monad.pure(())

    var ptr = start
    while ptr < finish && !needsEscape(chars.charAt(ptr)) do
      ptr += 1

    if ptr >= finish then return stream.write(chars, start, finish)

    if ptr == start then
      return write(stream, chars.charAt(start)) >=||
        writePart(stream, chars, start + 1, finish)

    stream.write(chars, start, ptr) >=||
      write(stream, chars.charAt(ptr)) >=||
      writePart(stream, chars, ptr + 1, finish)
  }


  /** Writes the whole character sequence as a JSON string. */
  def write[M[_]: Monad, S: DefaultWriter.In[M]](stream: S, str: CharSequence): M[Unit] =
    stream.write(STRING_BOUNDARY) >=|| writePart(stream, str) >=|| stream.write(STRING_BOUNDARY)


  /** Writes a simple single escaped character. */
  private def writeSimpleEscape[M[_]: Monad, S: CharWriter.In[M]](stream: S, escaped: Char): M[Unit] =
    stream.write('\\') >=|| stream.write(escaped)


  /** Writes character as an unicode escape. */
  private def writeUnicodeEscape[M[_]: Monad, S: CharWriter.In[M]](stream: S, chr: Char): M[Unit] =
    stream.write('\\') >=||
    stream.write('u') >=||
    stream.write(HEX_DIGITS((chr >> 12) & 0x0F)) >=||
    stream.write(HEX_DIGITS((chr >> 8) & 0x0F)) >=||
    stream.write(HEX_DIGITS((chr >> 4) & 0x0F)) >=||
    stream.write(HEX_DIGITS(chr & 0x0F))
}
