package io.github.maxkar
package http.server.jetty.qos

import http.server.api.Cookie

import scala.jdk.CollectionConverters.*


/**
 * Operations that deal with some (side-)effects upon the request context.
 */
private object Effects {
  /** Adds the headers to be unconditionally applied to the resulting request. */
  final class AddHeaders[Qos](headers: Seq[(String, String)])
      extends Operation.ContextOperation[Qos, Unit] {
    override def perform(context: RequestContext[Qos]): Unit =
      context.extraHeaders = context.extraHeaders ++ headers
  }


  /** Adds the cookie to be set after request is complete. */
  final class AddCookie[Qos](cookie: Cookie)
      extends Operation.ContextOperation[Qos, Unit] {
    override def perform(context: RequestContext[Qos]): Unit =
      context.cookies = cookie +: context.cookies
  }


  /** Invokes the cleaner. */
  final class InvokeCleaner[Qos](cleaner: Cleaner)
      extends Operation.ContextOperation[Qos, Unit] {
    override def perform(context: RequestContext[Qos]): Unit = {
      cleaner.unregister(context)
      cleaner.performCleanup()
    }
  }


  /** Registers the cleaner and returns the given result. */
  final class AddCleaner[Qos, R](cleaner: Cleaner, res: R)
      extends Operation.ContextOperation[Qos, R] {
    override def perform(context: RequestContext[Qos]): R = {
      cleaner.register(context)
      res
    }
  }


  /** Retrieves request method. */
  final class GetMethod[Qos] extends Operation.ContextOperation[Qos, String] {
    override def perform(context: RequestContext[Qos]): String =
      context.baseRequest.getMethod()
  }


  /** Retrieves header names. */
  final class GetHeaderNames[Qos] extends Operation.ContextOperation[Qos, Seq[String]] {
    override def perform(context: RequestContext[Qos]): Seq[String] =
      context.baseRequest.getHeaderNames().asScala.toSeq
  }


  /** Retrieves header values for the given header name. */
  final class GetHeader[Qos](name: String) extends Operation.ContextOperation[Qos, Seq[String]] {
    override def perform(context: RequestContext[Qos]): Seq[String] =
      context.baseRequest.getHeaders(name).asScala.toSeq
  }


  /** Retrieves values of the given cookie. */
  final class GetCookies[Qos](name: String) extends Operation.ContextOperation[Qos, Seq[String]] {
    override def perform(context: RequestContext[Qos]): Seq[String] =
      context.baseRequest
        .getCookies()
        .collect {
          case c if c.getName() == name => c.getValue()
        }
        .toSeq
  }


  /** Retrieves parameter names. */
  final class GetParameterNames[Qos] extends Operation.ContextOperation[Qos, Seq[String]] {
    override def perform(context: RequestContext[Qos]): Seq[String] =
      context.baseRequest.getParameterNames().asScala.toSeq
  }


  /** Retrieves parameter values for the given header name. */
  final class GetParameter[Qos](name: String) extends Operation.ContextOperation[Qos, Seq[String]] {
    override def perform(context: RequestContext[Qos]): Seq[String] =
      context.baseRequest.getParameterValues(name).toSeq
  }


  /** Retrieves QoS parameter. */
  final class GetQos[Qos] extends Operation.ContextOperation[Qos, Qos] {
    override def perform(context: RequestContext[Qos]): Qos =
      context.qos
  }
}
