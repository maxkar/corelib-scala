package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.coroutine.Coroutine

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

import org.eclipse.jetty.server.Request

/**
 * Module with quality-of-service support for requests.
 * @tparam Qos User-defineable quality-of-service parameter.
 * @param routine routine implementation for the QoS.
 * @param processing processing implementation.
 * @param routing routing implementation.
 * @param workThreads working threads - used for awaiting termination.
 */
final class Module[Qos] private(
      routine: Coroutine[HQ.Suspension[Qos]],
      processing: Processing[HQ.Step[Qos]],
      routing: Route[HQ.Step[Qos]],
      control: RequestControl,
      routineExecutor: RoutineExecutor[Qos],
      defaultQos: Qos,
      workThreads: Seq[Thread],
    ):

  /** Suspension - how the execution could be paused. */
  private type Suspension[T] = HQ.Suspension[Qos][T]

  /**
   * Type of the evaluation/execution in this module. Typeclass instances
   * like monad or process are defined for this type constructor.
   */
  opaque type Step[T] = HQ.Step[Qos][T]

  /** Result of the execution. */
  private type StepResult[T] = HQ.Step[Qos][T]

  /** Counter for generating request IDs. */
  private val requestSerial = new AtomicLong()

  /** Implementation of the monad for the Step. */
  given monadInstance: Monad[Step] = routine.monadInstance

  /** Request processing instance. */
  given processingInstance: Processing[Step] = processing

  /** Implementation of the route typeclass for our monad. */
  given routeInstance: Route[Step] = routing

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


  /** Stops the module and awaits the termination. */
  def stop(): Unit =
    if !control.requestTermination() then return
    control.awaitTermination()

    /* Send the "kill" signal for the threads. */
    for i <- 1 to workThreads.size do
      routineExecutor.continueRequest(
        new RequestContext[Qos](null, -1, defaultQos, Nil, Nil, null)
      )
    workThreads.foreach(_.join())
  end stop


  /**
   * Returns number of requests being known by this module.
   *
   * The number returned includes requests reported by `liveRequestCount` (being actively
   * executed by this module), `queuedRequestCount` (waiting to be executed when a thread
   * is available) and requests currently executed by another subsystem (for example, waiting
   * or being processed by Database Access layer).
   */
  def activeRequestCount: Int =
    control.getActiveRequestCount()


  /**
   * Returns number of requests that are being actively processed by this module and
   * are consuming CPU.
   */
  def liveRequestCount: Int =
    routineExecutor.getLiveRequestCount()


  /**
   * Returns number of requests queued for processing. These requests require some computing
   * by this module (routing and processing) and not some external one.
   */
  def queuedRequestCount: Int =
    routineExecutor.getPendingRequestCount()


  /**
   * Accepts a new jetty request for processing.
   * @param req request to process.
   * @param path request path that should be used for routing.
   * @param proc request handling routine.
   */
  private[qos] def processRequest(req: Request, path: List[String], proc: Step[Response]): Unit =
    req.setHandled(true)

    if !control.shouldProcessRequest() then
      abortFast(req)
      return

    req.startAsync()
    val ctx =
      new RequestContext[Qos](
        baseRequest = req,
        serial = requestSerial.incrementAndGet(),
        qos = defaultQos,
        initialRequestPath = path,
        effectivePath = path,
        nextSteps = proc
      )
    routineExecutor.continueRequest(ctx)
  end processRequest


  /**
   * Aborts the processing without invoking all the machinery (i.e. synchronously).
   */
  private inline def abortFast(req: Request): Unit =
    val resp = req.getResponse()
    resp.setStatus(503)
  end abortFast

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

    val routine = new Coroutine[HQ.Suspension[Qos]]
    implicit val processing = ProcessingImpl(routine)
    val routing = RouteImpl(routine, processing, errors, knownMethods)
    val control = new RequestControl()
    val queue =
      new PriorityBlockingQueue[RequestContext[Qos]](
        50,
        RequestContext.requestOrdering[Qos],
      )
    val routineExecutor = new RoutineExecutor(routine, control, queue, errors, sensor)
    val handler = new Runnable() {
      override def run(): Unit =
        routineExecutor.runAll()
    }

    val threads =
      Seq.fill(workThreads) { threadFactory.newThread(handler) }
    threads.foreach(_.start())

    new Module(
      routine,
      processing,
      routing,
      control,
      routineExecutor,
      defaultQos,
      threads
    )
  end apply
end Module
