package io.github.maxkar
package http.server.api

import java.nio.ByteBuffer
import java.nio.charset.Charset

import scala.language.implicitConversions

/** Simple encoders for server-side errors. */
trait Errors {
  /**
   * Encodes "path not found" error.
   * @param fullPath full request path (but only path) that was received.
   * @param unconsumed the section of the path that was won consumed by the
   *   routing. This is the part that was not mapped by the routing code. Note
   *   that it does not have to be a proper tail or subset of the `fullPath` as
   *   this section may be re-written by the routing code.
   */
  def pathNotFound(fullPath: List[String], unconsumed: List[String]): Response


  /**
   * Encodes the "unsupported method error".
   * @param method request method used.
   * @param supportedMethods methods that are actually supported by the handler.
   */
  def unsupportedMethod(method: String, supportedMethods: Iterable[String]): Response


  /**
   * The request is missing required header.
   * @param header header that was required but is missing.
   */
  def missingHeader(header: String): Response


  /**
   * Illegal header value occured.
   * @param header name of the header.
   * @param values values that were attempted to be parsed.
   * @param message message that somehow explains the reason why header is not valid.
   */
  def invalidHeader(header: String, values: Seq[String], message: String): Response


  /**
   * Required (request) parameter is missing.
   * @param parameter name of the missing parameter.
   */
  def missingParameter(parameter: String): Response


  /** Length of the input stream content exceedes the one allowed by the API. */
  def byteLengthExceeded(length: Long): Response


  /**
   * Internal server error occured.
   * @param ref unique reference that could be used to look-up error details on the server.
   */
  def internalError(ref: String): Response
}


object Errors {
  /** Hexadecimal characters. */
  private val hexChars = "0123456789ABCDEF"


  /** Minimal errors - just set the code but no explanation behind it. */
  object Minimal extends Errors {
    override def pathNotFound(fullPath: List[String], unconsumed: List[String]): Response =
      Response(404)()

    override def missingHeader(header: String): Response =
      Response(400)()

    override def missingParameter(header: String): Response =
      Response(400)()

    override def invalidHeader(header: String, values: Seq[String], message: String): Response =
      Response(400)()

    override def unsupportedMethod(method: String, supportedMethods: Iterable[String]): Response =
      Response(405, "Allow" -> supportedMethods.mkString(","))()

    override def byteLengthExceeded(length: Long): Response = Response(413)()

    override def internalError(ref: String): Response = Response(500)()
  }


  /** Simple text errors - just provide a text message and nothing else. */
  object SimpleText extends Errors {
    override def pathNotFound(fullPath: List[String], unconsumed: List[String]): Response =
      Response.text(404)("The requested path was not found on this server")


    override def missingHeader(header: String): Response =
      Response.text(400)(s"Missing required header ${header}")

    override def invalidHeader(header: String, values: Seq[String], message: String): Response =
      Response.text(400)(s"Illegal value of the ${header}: ${message}")

    override def missingParameter(parameter: String): Response =
      Response.text(400)(s"Missing required parameter ${parameter}")

    override def unsupportedMethod(method: String, supportedMethods: Iterable[String]): Response =
      Response.text(405, "Allow" -> supportedMethods.mkString(","))(
        s"The request method ${method} is not supported for the given request path"
      )

    override def internalError(ref: String): Response =
      Response.text(500)(s"An internal error has occured. If the error persists, please provide support team with the following code: ${ref}")

    override def byteLengthExceeded(length: Long): Response =
      Response.text(413, "Max-Byte-Length" -> length.toString())(
        s"Request size exceedes the maximum allowed size of ${length} bytes"
      )
  }
}
