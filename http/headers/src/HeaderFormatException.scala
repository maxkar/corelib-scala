package io.github.maxkar
package http.headers

/** Exception denoting an issue with the header format. */
final case class HeaderFormatException(message: String) extends Exception
