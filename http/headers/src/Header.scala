package io.github.maxkar
package http.headers

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
  def decodeFromString(values: Seq[String]): Either[String, T]


  /** Encodes (domain) value of the header into the transport form. */
  def encodeToString(value: T): String


  /**
   * A nice syntax arrow for those who would like to use "rich headers"
   * to generate output values.
   */
  final def -->(value: T): (String, String) =
    name -> encodeToString(value)
end Header