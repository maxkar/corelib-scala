package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.typeclass.Lift

import http.server.api.Route
import http.server.api.Response


/** Handler for deferring qos (into some buffer). */
final class DeferredQosHandler[B[_], W[_]: Monad: Route: Lift.From[B]](
      setQos: Int => W[Unit],
      asyncWait: Int => B[Int],
    ) {

  /** Handles the request. */
  val handle: W[Response] =
    Route.path {
      case x =>
        Route.method {
          case "GET" =>
            for
              id <- Route.getHeader("X-Id")
              iid = id.toInt
              _ <- setQos(iid)
              rv <- Lift(asyncWait(iid))
            yield
              Response.text(200)(s"${id}:${rv}")
        }
    }
}
