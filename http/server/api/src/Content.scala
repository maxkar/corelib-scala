package io.github.maxkar
package http.server.api

import fun.typeclass.Monad

import java.nio.charset.Charset

/**
 * Content that could be output by the response. In theory all the content
 * may be represented as a set of bytes (during the processing time). However
 * this may require additional memory. For example, a list of N elements would
 * require a memory amount proportional to N for the output buffer. Writing
 * things directly into output (or intermediate buffer) will require only
 * constant amount of memory.
 *
 * Character output is handled separately as the server may like to manage
 * encoding buffers internally.
 */
enum Content:
  /** No real content to output. */
  case Empty
  /** Bytes that could be output into the stream. */
  case Bytes(body: Content.ByteBody)
  /** Characters that should be output (and the output encoding). */
  case Chars(body: Content.CharBody, charset: Charset)
end Content

object Content:
  /** Writeable byte-oriented content body. */
  trait ByteBody:
    /**
     * Outputs the content into the given output stream according
     * to the desired output discipline.
     */
    def writeTo[M[_]: Monad](stream: ByteOutputStream[M]): M[Unit]
  end ByteBody


  /** Writeable character-oriented content body. */
  trait CharBody:
    /**
     * Outputs the content into the given output stream according
     * to the desired output discipline.
     */
    def writeTo[M[_]: Monad](stream: CharOutputStream[M]): M[Unit]
  end CharBody


  /** Creates a new content from string. */
  def apply(data: String, encoding: String = "UTF-8"): Content =
    Content.Chars(
      new Content.CharBody {
        override def writeTo[M[_]: Monad](stream: CharOutputStream[M]): M[Unit] =
          stream.write(data)
      },
      Charset.forName(encoding)
    )


  /** Creates a new content from bytes. */
  def apply(data: Array[Byte]): Content =
    Content.Bytes(
      new Content.ByteBody {
        override def writeTo[M[_]: Monad](stream: ByteOutputStream[M]): M[Unit] =
          stream.write(data)
      }
    )


  /** Automatic conversion from strings to content to make life easier. */
  given stringConversion: Conversion[String, Content] with
    override def apply(x: String): Content = Content(x, "UTF-8")
  end stringConversion


  /** Automatic conversion from byte arrays to content to make life easier. */
  given byteArrayConversion: Conversion[Array[Byte], Content] with
    override def apply(x: Array[Byte]): Content = Content(x)
  end byteArrayConversion
end Content
