package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.coroutine.Coroutine

import http.server.api.Cookie
import http.server.api.Response
import http.server.api.Processing
import http.server.api.Route
import http.server.api.ResourceCleaner
import http.server.api.NegotiableErrors

import http.server.toolkit.BaseRoute


/**
 * Module with quality-of-service support for requests.
 * @tparam Qos User-defineable quality-of-service parameter.
 */
final class Module[Qos] private(
      errors: NegotiableErrors,
      knownMethods: Iterable[String],
    ):

  /** Suspension - how the execution could be paused. */
  private type Suspension[T] = Operation[Qos, T]

  /**
   * Type of the evaluation/execution in this module. Typeclass instances
   * like monad or process are defined for this type constructor.
   */
  opaque type Step[T] = Coroutine.Routine[Suspension, T]

  /** Result of the execution. */
  private type StepResult[T] = Coroutine.RunResult[Suspension, T]

  /** Coroutine module with all the typeclasses, etc... */
  private val routine = new Coroutine[Suspension]

  /** Implementation of the monad for the Step. */
  given monadInstance: Monad[Step] = routine.monadInstance


  /** Request processing instance. */
  given processingInstance: Processing[Step] with
    override def abort[T](resp: Response): Step[T] =
      routine.suspend(Operation.Abort(resp))

    override def addHeaders(headers: Seq[(String, String)]): Step[Unit] =
      routine.suspend(Effects.AddHeaders(headers))

    override def setCookie(cookie: Cookie): Step[Unit] =
      routine.suspend(Effects.AddCookie(cookie))

    override def cleanup(cleaner: => Unit): Step[ResourceCleaner[Step]] =
      val c = new Cleaner(() => cleaner)
      val ret = new ResourceCleanerImpl(routine.suspend(Effects.InvokeCleaner(c)))
      routine.suspend(Effects.AddCleaner(c, ret))
    end cleanup


    override def withResource[R](resource: R, cleanup: R => Unit): Step[R] =
      val c = new Cleaner(() => cleanup(resource))
      routine.suspend(Effects.AddCleaner(c, resource))
    end withResource


    override def withCleanableResource[R](
          resource: R,
          cleanup: R => Unit,
        ): Step[(R, ResourceCleaner[Step])] =
      val c = new Cleaner(() => cleanup(resource))
      val ret = new ResourceCleanerImpl(routine.suspend(Effects.InvokeCleaner(c)))
      routine.suspend(Effects.AddCleaner(c, (resource, ret)))
    end withCleanableResource
  end processingInstance


  /** Implementation of the route typeclass for our monad. */
  given routeInstance: Route[Step] = new BaseRoute[Step]:
    override protected def abort[T](response: Response): Step[T] =
      processingInstance.abort(response)

    override protected val errors: NegotiableErrors = Module.this.errors

    override protected val knownMethods: Iterable[String] = Module.this.knownMethods

    override def path[T](fn: PartialFunction[List[String], Step[T]]): Step[T] =
      routine.suspend(new Operation.ComplexContextOperation[Qos, T] {
        override def perform(context: RequestContext[Qos]): Step[T] =
          doRoute(context.initialRequestPath, context.effectivePath, fn)
      })

    override def continue[T](unconsumedPath: List[String], handler: Step[T]): Step[T] =
      routine.suspend(new Operation.ComplexContextOperation[Qos, T] {
        override def perform(context: RequestContext[Qos]): Step[T] =
          context.effectivePath = unconsumedPath
          handler
        end perform
      })


    /** Cached implementation of the "get method" functionality. */
    private val getMethodInstance: Step[String] = routine.suspend(Effects.GetMethod())
    override def getMethod(): Step[String] = getMethodInstance


    /** Cached instance of "get all header names". */
    private val getHeadersInstance: Step[Seq[String]] = routine.suspend(Effects.GetHeaderNames())
    override def getHeaderNames(): Step[Seq[String]] = getHeadersInstance


    override def getHeaders(name: String): Step[Seq[String]] =
      routine.suspend(Effects.GetHeader(name))

    override def getCookies(name: String): Step[Seq[String]] =
      routine.suspend(Effects.GetCookies(name))


    /** Cached instance of "get all parameters names". */
    private val getParametersInstance: Step[Seq[String]] = routine.suspend(Effects.GetParameterNames())
    override def getParameterNames(): Step[Seq[String]] =  getParametersInstance

    override def getParameters(name: String): Step[Seq[String]] =
      routine.suspend(Effects.GetParameter(name))

    override def getBodyAsBytes(limit: Long): Step[Array[Byte]] =
      routine.suspend(Operation.ReadInputBytes(limit))
  end routeInstance
end Module


object Module:
  /**
   * Creates a new module with Quality-of-service support.
   * @tparam Qos user-driven way to describe quality of service.
   * @param errors error handlers used by this server.
   * @param knownMethods server-wide HTTP methods (the ones that
   *   may be reasonably expected from any handler).
   */
  def apply[Qos: Ordering](
        errors: NegotiableErrors,
        knownMethods: Iterable[String] = Route.defaultMethod,
      ): Module[Qos] =
    new Module(errors, knownMethods)
end Module
