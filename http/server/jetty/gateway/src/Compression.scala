package io.github.maxkar
package http.server.jetty.gateway

import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.gzip.GzipHandler


object Compression {
  /**
   * Enables GZIP compression on the given handler.
   * @param handler handler to add compression for.
   * @param methods methods that would support compression (compression output).
   */
  def apply(handler: Handler, methods: Iterable[String]): Handler = {
    val gz = new GzipHandler()
    gz.setHandler(handler)
    gz.setExcludedMethods()
    gz.setIncludedMethods(methods.toArray*)
    gz
  }
}
