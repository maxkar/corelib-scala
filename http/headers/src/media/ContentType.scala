package io.github.maxkar
package http.headers.media

import http.headers.Header
import http.headers.ValueParser
import http.headers.ValueWriter
import http.headers.HeaderFormatException

/** Content type header implementation. */
object ContentType extends Header[MediaType] {
  override val name: String = "Content-Type"

  override def encodeToString(value: MediaType): String = {
    val sb = new StringBuilder()
    sb.append(value.category)
    sb.append('/')
    sb.append(value.subtype)
    ValueWriter.writeParameters(value.parameters, sb)
    sb.toString()
  }


  override def decodeFromString(values: Seq[String]): MediaType = {
    if values.isEmpty then
      throw new HeaderFormatException("No Content-Type header was provided")
    if values.length > 1 then
      throw new HeaderFormatException("More than one Content-Type header")

    var reader = new ValueParser(values(0))
    reader.skipWhitespaces()
    val category = reader.readToken()
    reader.expectAndRead('/')
    val subtype = reader.readToken()
    val params = reader.readParameters()
    reader.expectEof()
    MediaType(category, subtype, params*)
  }
}
