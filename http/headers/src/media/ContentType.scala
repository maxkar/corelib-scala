package io.github.maxkar
package http.headers.media

import http.headers.Header
import http.headers.ValueParser
import http.headers.ValueWriter

/** Content type header implementation. */
object ContentType extends Header[MediaType]:
  override val name: String = "Content-Type"

  override def encodeToString(value: MediaType): String =
    val sb = new StringBuilder()
    sb.append(value.category)
    sb.append('/')
    sb.append(value.subtype)
    ValueWriter.writeParameters(value.parameters, sb)
    sb.toString()
  end encodeToString


  override def decodeFromString(values: Seq[String]): Either[String, MediaType] =
    if values.isEmpty then
      return Left("Missing Content-Type header")

    if values.length > 1 then
      return Left("More than one Content-Type header")

    var reader = new ValueParser(values(0))
    reader.skipWhitespaces()
    for
      category <- reader.readToken()
      _ <- reader.expectAndRead('/')
      subtype <- reader.readToken()
      params <- reader.readParameters()
      _ <- reader.expectEof()
    yield
      MediaType(category, subtype, params*)
  end decodeFromString
end ContentType
