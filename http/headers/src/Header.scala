package io.github.maxkar
package http.headers

/**
 * Handler of some specific header that could be represented by a "native" scala type.
 * The header (instances) define specific conversion logic.
 */
trait Header[T] {
  /** Name of the header in the protocol. */
  val name: String


  /**
   * Attempts to parse request value(s) into the actual epresentation type.
   * This method **may** be called with an empty sequence.
   *
   * @return value of the parsed header.
   * @throws HeaderFormatException if values do not denote proper header.
   */
  def decodeFromString(values: Seq[String]): T


  /** Encodes (domain) value of the header into the transport form. */
  def encodeToString(value: T): String


  /**
   * A nice syntax arrow for those who would like to use "rich headers"
   * to generate output values.
   */
  final def -->(value: T): (String, String) =
    name -> encodeToString(value)
}
