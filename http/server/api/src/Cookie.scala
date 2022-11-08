package io.github.maxkar
package http.server.api

/** Cookie that could be set. */
case class Cookie(
  name: String,
  value: String,
  maxAge: Option[Int] = None,
  path: Option[String] = None,
  secure: Option[Boolean] = None,
  httpOnly: Option[Boolean] = None,
)
