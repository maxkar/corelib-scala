package io.github.maxkar
package json.writer

import fun.typeclass.Monad

import text.output.Stream
import java.io.OutputStream


/** String-related output functions. */
object Strings {
  /** String boundary. */
  val STRING_BOUNDARY: String = "\""

  /** Hex digits. */
  private val HEX_DIGITS = "0123456789ABCDEF"

  /** Lower unicode range. JSON requires these to be always escaped.  */
  val LOWER_UNICODE: Array[String] =
    (
      HEX_DIGITS.map(v => s"\\u000${v}") ++
      HEX_DIGITS.map(v => s"\\u001${v}")
    ).toArray

  /** Quote escape character. */
  val ESC_QUOTE = "\\\""

  /** Reverse slash escape character. */
  val ESC_RSLASH = "\\\\"

  /** Bell (\b) escape character. */
  val ESC_BELL = "\\b"

  /** Form feed escape character. */
  val ESC_FF = "\\f"

  /** Carriage return escape character. */
  val ESC_CR = "\\r"

  /** Line feed escape character. */
  val ESC_LF = "\\n"

  /** Tab escape character. */
  val ESC_TAB = "\\t"

  /** Writes string boundary into the output stream. */
  def writeStringBoundary[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(STRING_BOUNDARY)


  /**
   * Outputs string content (no boundary characters) into the stream.
   * Performs necessary (minimally required) escaping of the output.
   *
   * This method may be used consequently on string parts if complete
   * string is not available for any reason.
   */
  def writeStringContent[M[_]: Monad](
        data: CharSequence,
        stream: Stream[M],
      ): M[Unit] =
    writeStringRest(data, 0, stream)


  /**
   * Writes the string including boundary characters.
   */
  def writeString[M[_]: Monad](
        data: CharSequence,
        stream: Stream[M],
      ): M[Unit] =
    for
      _ <- writeStringBoundary(stream)
      _ <- writeStringRest(data, 0, stream)
      res <- writeStringBoundary(stream)
    yield
      res


  /**
   * Outputs string content from the given position in the char sequence and
   * until the sequence end.
   */
  private def writeStringRest[M[_]: Monad](
        data: CharSequence,
        start: Int,
        stream: Stream[M]
      ): M[Unit] = {
    if data.length() <= start then
      return Monad.pure(())

    var ptr = start

    while ptr < data.length() do {
      data.charAt(ptr) match {
        case '"' =>
          return writeStringWithSpecial(data, start, ptr, ESC_QUOTE, stream)
        case '\\' =>
          return writeStringWithSpecial(data, start, ptr, ESC_RSLASH, stream)
        case '\b' =>
          return writeStringWithSpecial(data, start, ptr, ESC_BELL, stream)
        case '\f' =>
          return writeStringWithSpecial(data, start, ptr, ESC_FF, stream)
        case '\r' =>
          return writeStringWithSpecial(data, start, ptr, ESC_CR, stream)
        case '\n' =>
          return writeStringWithSpecial(data, start, ptr, ESC_LF, stream)
        case '\t' =>
          return writeStringWithSpecial(data, start, ptr, ESC_TAB, stream)
        case x if x < 0x0020 =>
          return writeStringWithSpecial(data, start, ptr, LOWER_UNICODE(x), stream)
        case other => ptr += 1
      }
    }

    stream.write(data.subSequence(start, ptr))
  }


  /**
   * Writes "regular" string part and then some special (escaped) character.
   */
  private def writeStringWithSpecial[M[_]: Monad](
        data: CharSequence,
        start: Int,
        regularEnd: Int,
        special: String,
        stream: Stream[M],
      ): M[Unit] = {
    var base =
      if start < regularEnd then
        stream.write(data.subSequence(start, regularEnd)).flatMap { _ =>
          stream.write(special)
        }
      else
        stream.write(special)

    val nextSectionStart = regularEnd + 1

    if nextSectionStart < data.length() then
      base.flatMap { _ =>
        writeStringRest(data, nextSectionStart, stream)
      }
    else
      base
  }
}
