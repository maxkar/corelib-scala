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
      routine: Coroutine[HQ.Suspension[Qos]],
      processing: Processing[HQ.Step[Qos]],
      override protected val errors: NegotiableErrors,
      override protected val knownMethods: Iterable[String],
    ) extends BaseRoute[HQ.Step[Qos]] {
  /** Just a synonym for making life easier. */
  private type Step[T] = HQ.Step[Qos][T]

  /** Cached implementation of the "get method" functionality. */
  private val getMethodInstance: Step[String] = routine.suspend(Effects.GetMethod())

  /** Cached instance of "get all header names". */
  private val getHeadersInstance: Step[Seq[String]] = routine.suspend(Effects.GetHeaderNames())

  /** Cached instance of "get all parameters names". */
  private val getParametersInstance: Step[Seq[String]] = routine.suspend(Effects.GetParameterNames())


  override protected def abort[T](response: Response): Step[T] =
    processing.abort(response)


  override def path[T](fn: PartialFunction[List[String], Step[T]]): Step[T] =
    routine.suspend(new Operation.ComplexContextOperation[Qos, T] {
      override def perform(context: RequestContext[Qos]): Step[T] =
        doRoute(context.initialRequestPath, context.effectivePath, fn)
    })


  override def continue[T](unconsumedPath: List[String], handler: Step[T]): Step[T] =
    routine.suspend(new Operation.ComplexContextOperation[Qos, T] {
      override def perform(context: RequestContext[Qos]): Step[T] = {
        context.effectivePath = unconsumedPath
        handler
      }
    })

  override def getMethod(): Step[String] = getMethodInstance

  override def getHeaderNames(): Step[Seq[String]] = getHeadersInstance

  override def getHeaders(name: String): Step[Seq[String]] =
    routine.suspend(Effects.GetHeader(name))

  override def getCookies(name: String): Step[Seq[String]] =
    routine.suspend(Effects.GetCookies(name))

  override def getParameterNames(): Step[Seq[String]] =  getParametersInstance

  override def getParameters(name: String): Step[Seq[String]] =
    routine.suspend(Effects.GetParameter(name))

  override def getBodyAsBytes(limit: Long): Step[Array[Byte]] =
    routine.suspend(Operation.ReadInputBytes(limit))
}
