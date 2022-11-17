package io.github.maxkar
package http.server.jetty.qos

import http.server.api.Response

import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.{Handler => JettyHandler}
import org.eclipse.jetty.server.handler.AbstractHandler

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse


object Handler:
  /**
   * Creates a new jetty handler for the given modul. The handler will
   * execute the request on the module's thread pool and according module's QoS
   * policies.
   */
  def apply[Qos](module: Module[Qos], handler: module.Step[Response]): JettyHandler =
    new AbstractHandler {
      override def handle(
            target: String,
            baseRequest: Request,
            request: HttpServletRequest,
            response: HttpServletResponse,
          ): Unit =
        var path = target.split("/").toList
        if path.headOption.contains("") then path = path.tail
        module.processRequest(baseRequest, path, handler)
      end handle
    }
  end apply
end Handler
