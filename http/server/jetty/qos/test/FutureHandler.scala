package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.typeclass.Lift

import http.server.api.Route
import http.server.api.Response

import scala.concurrent.Future


/** Very basic handler for tests (future resolution). */
final class FutureHandler[W[_]: Monad: Route: Lift.From[Future]](calculation: Future[String]):

  /** Handles the request. */
  val handle: W[Response] =
    Route.path {
      case x =>
        Route.method {
          case "GET" =>
            for
              resp <- Lift(calculation)
            yield
              Response.text(200)(resp)
            end for
        }
    }
end FutureHandler


