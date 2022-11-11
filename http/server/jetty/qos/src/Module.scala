package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.coroutine.Coroutine
import fun.coroutine.Coroutine.RunResult

import java.util.concurrent.BlockingQueue
import java.util.concurrent.PriorityBlockingQueue
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicLong

import http.server.api.Cookie
import http.server.api.Response
import http.server.api.Processing
import http.server.api.Route
import http.server.api.ResourceCleaner
import http.server.api.NegotiableErrors

import http.server.toolkit.BaseRoute

import scala.jdk.CollectionConverters.*

import org.eclipse.jetty.server.Request

/**
 * Module with quality-of-service support for requests.
 * @tparam Qos User-defineable quality-of-service parameter.
 */
final class Module[Qos] private(
      errors: NegotiableErrors,
      knownMethods: Iterable[String],
      defaultQos: Qos,
      private[qos] val sensor: Sensor,
      queue: BlockingQueue[RequestContext[Qos]],
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

  /** Counter for generating request IDs. */
  private val requestSerial = new AtomicLong()


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


  /** Cached instance of "Get Quality of Service". */
  private val getQosInstance: Step[Qos] = routine.suspend(Effects.GetQos())
  /** Retrieves current value of the "quality of service" parameter. */
  def getQos(): Step[Qos] = getQosInstance

  /**
   * Adjusts the (program-defined) quality of service for the current request.
   * @param newQos new "quality-of-service" parameter that affects how request
   *   processing is being scheduled.
   */
  def setQos(newQos: Qos): Step[Unit] =
    routine.suspend(Operation.SetQos(newQos))


  /**
   * Accepts a new jetty request for processing.
   * @param req request to process.
   * @param path request path that should be used for routing.
   * @param proc request handling routine.
   */
  private[qos] def processRequest(req: Request, path: List[String], proc: Step[Response]): Unit =
    val ctx =
      new RequestContext[Qos](
        baseRequest = req,
        serial = requestSerial.incrementAndGet(),
        qos = defaultQos,
        initialRequestPath = path,
        effectivePath = path,
        nextSteps = proc
      )
    continueRequest(ctx)
  end processRequest


  /** Runs the main loop. */
  private def runAll(): Unit =
    while true do
      try
        runContext(queue.take())
      catch
        case e: Throwable =>
          sensor.genericError(e)
    end while
  end runAll


  /**
   * Continues processing of the given monad according to all the defined rules.
   * This method may be called from **any** thread.
   */
  private def continueRequest(context: RequestContext[Qos]): Unit =
    queue.add(context)


  /** Runs the next steps of the processing. */
  private def runContext(context: RequestContext[Qos]): Unit =
    var md = context.nextSteps
    context.nextSteps = null
    try
      while true do
        routine.run(md) match
          case RunResult.Finished(resp) =>
            OutputOperation(this, context, resp)
            return
          case RunResult.Suspended(Operation.Abort(resp), _) =>
            OutputOperation(this, context, resp)
            return
          case RunResult.Suspended(Operation.ReadInputBytes(limit), cont) =>
            InputOperation(this, context, limit, cont)
            return
          case RunResult.Suspended(Operation.SetQos(qos), cont) =>
            context.qos = qos
            /* Bind this as "cont(x)" may be heavy computation. */
            context.nextSteps = monadInstance.bind(monadInstance.pure(()), cont)
            /* Re-schedule the request with new QoS. This may de-prioritize
             * the current one and give some other request a chance to be executed.
             */
            continueRequest(context)
            return
          case RunResult.Suspended(x: Operation.ContextOperation[Qos, _], cont) =>
            md = cont(x.perform(context))
          case RunResult.Suspended(x: Operation.ComplexContextOperation[Qos, _], cont) =>
            md = monadInstance.bind(x.perform(context), cont)
        end match
      end while
    catch
      case e: Throwable => raiseInternalError(context, e)
  end runContext


  /** Raises the "request size too large" response for the given request. */
  private[qos] def raiseRequestSizeTooLarge(context: RequestContext[Qos], limit: Long): Unit =
    context.nextSteps =
      processingInstance.abort(
        errors.byteLengthExceeded(context.baseRequest.getHeaders("Accept").asScala.toSeq, limit)
      )
    continueRequest(context)
  end raiseRequestSizeTooLarge


  /** Completes input operation for the given request. */
  private[qos] def completeInput(
        context: RequestContext[Qos],
        data: Array[Byte],
        nextFun: Array[Byte] => Step[Response],
      ): Unit =
    context.nextSteps = monadInstance.bind(monadInstance.pure(data), nextFun)
    continueRequest(context)
  end completeInput



  /**
   * Raises an internal error and aborts request processing.
   * This may be called from **any** thread.
   */
  private[qos] def raiseInternalError(context: RequestContext[Qos], e: Throwable): Unit =
    /* First, try to generate response.
     * Everything may fail so we are trying to be extra careful. */
    val errResponse =
      try
        val ref = sensor.internalError(context.serial, e)
        errors.internalError(context.baseRequest.getHeaders("Accept").asScala.toSeq, ref)
      catch
        case e: Throwable =>
          try
            sensor.invisibleError(context.serial, e)
          catch
            case e: Throwable => ()
          end try
          Response(500)()
      end try

    /* Now, complete the request. Depending on where it happened it may be either
     * Jetty thread (input routine) or Internal Thread (raised as part of the
     * coroutine evaluation). We handle both failures on the same thread where
     * it happened.
     */
    OutputOperation(this, context, errResponse)
  end raiseInternalError


  /** Handles an issue with the context output. */
  private[qos] def outputError(context: RequestContext[Qos], e: Throwable): Unit =
    try
      sensor.invisibleError(context.serial, e)
    finally
      finish(context)
  end outputError


  /**
   * "Finishes" processing the context by notifying servlet about processing being
   * complete and runs the cleaners associated with the request.
   */
  private[qos] def finish(context: RequestContext[Qos]): Unit =
    try
      context.baseRequest.getAsyncContext().complete()
    finally
      cleanup(context)
  end finish


  /**
   * Cleans-up the context by running all the registered cleaners.
   */
  private def cleanup(context: RequestContext[Qos]): Unit =
    var cleaner = context.cleaner
    context.cleaner = null
    while cleaner != null do
      val nextCleaner = cleaner.next
      try
        cleaner.drop()
        cleaner.performCleanup()
      catch
        case e: Throwable =>
          try
            sensor.invisibleError(context.serial, e)
          catch
            case e: Throwable =>
              /* Explicitly ignore. We don't want to cause resource leak if
               * there are some sensor issues. Or at least we should make a best
               * effort to cleanup those resources.
               */
              ()
          end try
      end try
      cleaner = nextCleaner
    end while
  end cleanup
end Module


object Module:
  /**
   * Creates a new module with Quality-of-service support.
   * @tparam Qos user-driven way to describe quality of service.
   * @param errors error handlers used by this server.
   * @param knownMethods server-wide HTTP methods (the ones that
   *   may be reasonably expected from any handler).
   * @param defaultQos default Quality of Service level that set for every
   *   new incoming request.
   * @param threadFactory thread factory used to create workers.
   * @param workThreads number of work threads to start for request processing.
   * @param sensor error sensor that is notified about execution errors.
   */
  def apply[Qos: Ordering](
        errors: NegotiableErrors,
        knownMethods: Iterable[String] = Route.defaultMethods,
        defaultQos: Qos,
        threadFactory: ThreadFactory,
        workThreads: Int,
        sensor: Sensor,
      ): Module[Qos] =

    val queue =
      new PriorityBlockingQueue[RequestContext[Qos]](
        50,
        RequestContext.requestOrdering[Qos],
      )

    val module = new Module(errors, knownMethods, defaultQos, sensor, queue)
    val handler = new Runnable() {
      override def run(): Unit =
        module.runAll()
    }

    for i <- 1 to workThreads do
      threadFactory.newThread(handler).start()
    module
  end apply
end Module
