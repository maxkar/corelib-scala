package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import http.server.api.Route
import http.server.api.Response


/** Very basic handler for tests. */
final class SampleHandler[W[_]: Monad: Route](setQos: Boolean => W[Unit]) {

  /** Handles the request. */
  val handle: W[Response] =
    Route.path {
      case x =>
        Route.method {
          case "GET" =>
            for
              maybeSuperHeader <- Route.getOptHeader("X-Admin")
              isAdmin = maybeSuperHeader == Some("true")
              _ <- setQos(isAdmin)
            yield {
              // SLOOOW process
              Thread.sleep(2000)
              Response.text(200)("Hello, world!")
            }
        }
    }
}
