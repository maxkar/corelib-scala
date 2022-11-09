package io.github.maxkar
package http.server.jetty.qos

import http.server.api.Cookie

import org.eclipse.jetty.server.Request

/**
 * Context of one HTTP request that is being processed.
 * @param baseRequest jetty request that was started. May be used by the process
 *   to mark it as completed or to use it for input and output.
 * @param serial serial request number - incoming requests get sequentially incrementing
 *   numbers (but the starting value is not specified).
 * @param qos quality-of service parameter that may be set by user.
 * @param initialRequestPath initial path to the request (not modified by the router).
 * @param effectivePath effective path used for Route.path routing.
 * @param extraHeaders extra headers that should be added to the response (side effect).
 * @param cleaner a node in the "cleanup" chain.
 */
final class RequestContext[Qos](
      val baseRequest: Request,
      val serial: Long,
      var qos: Qos,
      val initialRequestPath: List[String],
      var effectivePath: List[String],
      var extraHeaders: Seq[(String, String)] = Seq.empty,
      var cookies: Seq[Cookie] = Seq.empty,
      var cleaner: Cleaner = null
    )


object RequestContext:
  /** Ordering for context based on the Qos ordering. */
  given requestOrdering[Qos](using qorder: Ordering[Qos]): Ordering[RequestContext[Qos]] with
    override def compare(x: RequestContext[Qos], y: RequestContext[Qos]): Int =
      val qvalue = qorder.compare(x.qos, y.qos)
      if qvalue != 0 then qvalue
      else if x.serial < y.serial then -1
      else if x.serial > y.serial then 1
      else 0
    end compare
  end requestOrdering
end RequestContext
