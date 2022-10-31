package io.github.maxkar
package http.server.api

/**
 * Handler of some specific header that could be represented by a "native" scala type.
 * The header (instances) define specific conversion logic.
 */
trait Header[T]:
  /** Name of the header in the protocol. */
  val name: String


  /**
   * Attempts to parse request value(s) into the actual epresentation type.
   * This method **may** be called with an empty sequence.
   *
   * @return `Right(value)` if parsing was successfull or `Left(error)`
   *   if parsing was not successful.
   */
  def fromTransport(values: Seq[String]): Either[String, T]


  /** Encodes (domain) value of the header into the transport form. */
  def toTransport(value: T): String
end Header
