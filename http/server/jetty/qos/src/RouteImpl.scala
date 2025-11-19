package io.github.maxkar
package http.server.jetty.qos

import fun.coroutine.Coroutine

import http.server.api.Route

import http.server.api.Response
import http.server.api.Processing
import http.server.api.NegotiableErrors
import http.server.toolkit.BaseRoute


/** Implementation of the route typeclass. */
private final class RouteImpl[Qos](
      processing: Processing[HQ.Step[Qos]],
      override protected val errors: NegotiableErrors,
      override protected val knownMethods: Iterable[String],
    ) extends BaseRoute[HQ.Step[Qos]] {
  /** Just a synonym for making life easier. */
  private type Step[T] = HQ.Step[Qos][T]

  /** Cached implementation of the "get method" functionality. */
  private val getMethodInstance: Step[String] = Coroutine.call(Effects.GetMethod())

  /** Cached instance of "get all header names". */
  private val getHeadersInstance: Step[Seq[String]] = Coroutine.call(Effects.GetHeaderNames())

  /** Cached instance of "get all parameters names". */
  private val getParametersInstance: Step[Seq[String]] = Coroutine.call(Effects.GetParameterNames())


  override protected def abort[T](response: Response): Step[T] =
    processing.abort(response)


  override def path[T](fn: PartialFunction[List[String], Step[T]]): Step[T] =
    getPaths().flatMap((effectivePath, initialPath) => {
      doRoute(initialPath, effectivePath, fn)
    })


  override def continue[T](unconsumedPath: List[String], handler: Step[T]): Step[T] =
    setPath(unconsumedPath).flatMap(_ => handler)

  override def getMethod(): Step[String] = getMethodInstance

  override def getHeaderNames(): Step[Seq[String]] = getHeadersInstance

  override def getHeaders(name: String): Step[Seq[String]] =
    Coroutine.call(Effects.GetHeader(name))

  override def getCookies(name: String): Step[Seq[String]] =
    Coroutine.call(Effects.GetCookies(name))

  override def getParameterNames(): Step[Seq[String]] =  getParametersInstance

  override def getParameters(name: String): Step[Seq[String]] =
    Coroutine.call(Effects.GetParameter(name))

  override def getBodyAsBytes(limit: Long): Step[Array[Byte]] =
    Coroutine.call(Operation.ReadInputBytes(limit))

  private def getPaths(): Step[(List[String], List[String])] =
    Coroutine.call(new Operation.ContextOperation[Qos, (List[String], List[String])] {
      override def perform(context: RequestContext[Qos]): (List[String], List[String]) =
        (context.effectivePath, context.initialRequestPath)
    })

  private def setPath(newPath: List[String]): Step[Unit] =
    Coroutine.call(new Operation.ContextOperation[Qos, Unit] {
      override def perform(context: RequestContext[Qos]): Unit =
        context.effectivePath = newPath
    })
}
