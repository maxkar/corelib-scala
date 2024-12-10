
package io.github.maxkar
package http.server.api

import http.headers.HeaderFormatException
import http.headers.media.Accept
import http.headers.media.MediaType

import java.nio.ByteBuffer
import java.nio.charset.Charset


/**
 * Error handler that supports content negotiation protocol for the error(s)
 * being returned.
 *
 * @param lookup lookup data from supported (output) media type to the error
 *   handler that could generate appropriate message.
 * @param default hanlder to use when no content-based error handler is found.
 */
final class NegotiableErrors(lookup: Iterable[(MediaType, Errors)], default: Errors) {
  /**
   * Encodes "path not found" error.
   * @param accept accept specification(s) describing client's format preferences.
   * @param fullPath full request path (but only path) that was received.
   * @param unconsumed the section of the path that was won consumed by the
   *   routing. This is the part that was not mapped by the routing code. Note
   *   that it does not have to be a proper tail or subset of the `fullPath` as
   *   this section may be re-written by the routing code.
   */
  def pathNotFound(
        accept: Seq[String],
        fullPath: List[String],
        unconsumed:
         List[String],
      ): Response =
    negotiate(accept).pathNotFound(fullPath, unconsumed)


  /**
   * Encodes the "unsupported method error".
   * @param accept accept specification(s) describing client's format preferences.
   * @param method request method used.
   * @param supportedMethods methods that are actually supported by the handler.
   */
  def unsupportedMethod(
        accept: Seq[String],
        method: String,
        supportedMethods: Iterable[String],
      ): Response =
    negotiate(accept).unsupportedMethod(method, supportedMethods)


  /**
   * The request is missing required header.
   * @param accept accept specification(s) describing client's format preferences.
   * @param header header that was required but is missing.
   */
  def missingHeader(
        accept: Seq[String],
        header: String,
      ): Response =
    negotiate(accept).missingHeader(header)


  /**
   * Illegal header value occured.
   * @param accept accept specification(s) describing client's format preferences.
   * @param header name of the header.
   * @param values values that were attempted to be parsed.
   * @param message message that somehow explains the reason why header is not valid.
   */
  def invalidHeader(
        accept: Seq[String],
        header: String,
        values: Seq[String],
        message: String,
      ): Response =
    negotiate(accept).invalidHeader(header, values, message)


  /**
   * Required (request) parameter is missing.
   * @param accept accept specification(s) describing client's format preferences.
   * @param parameter name of the missing parameter.
   */
  def missingParameter(
        accept: Seq[String],
        parameter: String,
      ): Response =
    negotiate(accept).missingParameter(parameter)


  /**
   * Length of the input stream content exceedes the one allowed by the API.
   * @param accept accept specification(s) describing client's format preferences.
   * @param length maximum accepted length in bytes.
   */
  def byteLengthExceeded(
        accept: Seq[String],
        length: Long,
      ): Response =
    negotiate(accept).byteLengthExceeded(length)


  /**
   * Internal server error occured.
   * @param accept accept specification(s) describing client's format preferences.
   * @param ref unique reference that could be used to look-up error details on the server.
   */
  def internalError(
        accept: Seq[String],
        ref: String,
      ): Response =
    negotiate(accept).internalError(ref)


  /**
   * "Negotiates" error hanler based on the provided accept header. It tries to
   * get the highest-priority media type that matches an entry and the first
   * lookup entry that is accepted. If there is no such entry or `accept` headers
   * are not valid (i.e. mailformed) then default error handler is returned.
   */
  private def negotiate(accept: Seq[String]): Errors = {
    val baseClauses =
      try {
        Accept.decodeFromString(accept)
      } catch {
        /* We are already in error handler and should not cause new errors,
         * fallback to the default instead.
         */
        case _: HeaderFormatException => return default
      }

    val clauses = baseClauses.sorted

    val ci = clauses.iterator

    while ci.hasNext do {
      val clause = ci.next()

      val li = lookup.iterator
      while li.hasNext do {
        val item = li.next()
        if clause.accepts(item._1) then
          return item._2
      }
    }

    return default
  }
}
