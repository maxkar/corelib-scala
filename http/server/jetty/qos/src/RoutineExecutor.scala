package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.coroutine.Coroutine
import fun.coroutine.Coroutine.RunResult

import http.server.api.Response
import http.server.api.Processing
import http.server.api.NegotiableErrors

import java.util.concurrent.BlockingQueue
import java.util.concurrent.PriorityBlockingQueue

import scala.jdk.CollectionConverters.*

/**
 * Routine execution engine - how to run the request, how to execute
 * coroutine steps, etc...
 *
 * @param routine routine engine used.
 * @param control request control. It is NOT used to check if the
 *   request should be started. It is only notified when a request
 *   is complete.
 * @param queue request queue. If the queue is bounded, its capacity should
 *   be configured in conjunction with the `control` so that request cut-off
 *   is handled by the control and not the queue size.
 * @param errors error specification/handler.
 * @param sensor sensor for various events.
 */
private final class RoutineExecutor[Qos](
        routine: Coroutine[HQ.Suspension[Qos]],
        control: RequestControl,
        queue: BlockingQueue[RequestContext[Qos]],
        errors: NegotiableErrors,
        sensor: Sensor,
      )(
        using monadInstance: Monad[HQ.Step[Qos]],
        processingInstance: Processing[HQ.Step[Qos]]
      ):


  /**
   * Number of requests actively running routines (i.e. using the module's pool).
   */
  private val liveRequests = new java.util.concurrent.atomic.AtomicInteger()


  /**
   * Runs the main loop. May be called from multiple threads. Stops on seeing a request
   * with `null` element.
   */
  def runAll(): Unit =
    while true do
      try
        val elem = queue.take()
        if elem.baseRequest == null then
          return
        end if
        liveRequests.incrementAndGet()
        try
          runContext(elem)
        finally
          liveRequests.decrementAndGet()
      catch
        case e: Throwable => sensor.genericError(e)
    end while
  end runAll


  /**
   * Continues processing of the given monad according to all the defined rules.
   * This method may be called from **any** thread.
   */
  def continueRequest(context: RequestContext[Qos]): Unit =
    queue.add(context)


  /**
   * Continues request with the given next steps.
   * @param context context to continue execution for.
   * @param nextSteps steps to perform on the context.
   */
  def continueRequest(context: RequestContext[Qos], nextSteps: HQ.Step[Qos][Response]): Unit =
    context.nextSteps = nextSteps
    continueRequest(context)


  /**
   * Continues the requset where next steps are created by applying a function
   * to a given value (most common continuation from "resultful" calls).
   */
  def continueRequest[T](context: RequestContext[Qos], value: T, nextSteps: T => HQ.Step[Qos][Response]): Unit =
    context.nextSteps = Monad.pure(value) flatMap nextSteps
    continueRequest(context)


  /** Returns number of requests being actively processed. */
  def getLiveRequestCount(): Int =
    liveRequests.get()


  /** Returns number of peding requests. */
  def getPendingRequestCount(): Int =
    queue.size()


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
          case RunResult.Suspended(Operation.Raise(error), _) =>
            raiseInternalError(context, error)
            return
          case RunResult.Suspended(Operation.ReadInputBytes(limit), cont) =>
            InputOperation(this, context, limit, cont)
            return
          case RunResult.Suspended(Operation.RunCompletable(op, bound), cont) =>
            runCompletable(context, op, bound, cont)
            return
          case RunResult.Suspended(Operation.RunScheduled(op, bound), cont) =>
            runScheduled(context, op, bound, cont)
            return
          case RunResult.Suspended(Operation.SetQos(qos), cont) =>
            context.qos = qos
            /* Bind this as "cont(x)" may be heavy computation. */
            context.nextSteps = monadInstance.bind(monadInstance.pure(()), cont)
            /* Re-schedule the request with new QoS. This may de-prioritize
             * the current one and give some other request a chance to be executed.
             */
            continueRequest(context, (), cont)
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


  /**
   * Runs the completable. It is just a type-checked version of the inline implementation.
   */
  private inline def runCompletable[S[_], T](
        context: RequestContext[Qos],
        operation: S[T],
        bound: boundary.Completable[S],
        cont: T => HQ.Step[Qos][Response]
      ): Unit =
    bound.onComplete(operation,
      onSuccess = v =>
        continueRequest(context, v, cont),
      onFailure = t =>
        continueRequest(
          context,
          routine.suspend(Operation.Raise[Qos, Response](t))
        )
    )


  /**
   * Runs the scheduled. It is just a type-checked version of the inline implementation.
   */
  private inline def runScheduled[S[_], T](
        context: RequestContext[Qos],
        operation: S[T],
        bound: boundary.Scheduled[S, Qos],
        cont: T => HQ.Step[Qos][Response]
      ): Unit =
    bound(
      operation,
      context.qos,
      context.serial,
      onSuccess = v =>
        continueRequest(context, v, cont),
      onFailure = t =>
        continueRequest(
          context,
          routine.suspend(Operation.Raise[Qos, Response](t))
        )
    )


  /** Raises the "request size too large" response for the given request. */
  private[qos] def raiseRequestSizeTooLarge(context: RequestContext[Qos], limit: Long): Unit =
    continueRequest(
      context,
      processingInstance.abort(
        errors.byteLengthExceeded(context.baseRequest.getHeaders("Accept").asScala.toSeq, limit)
      )
    )
  end raiseRequestSizeTooLarge


  /** Completes input operation for the given request. */
  private[qos] def completeInput(
        context: RequestContext[Qos],
        data: Array[Byte],
        nextFun: Array[Byte] => HQ.Step[Qos][Response],
      ): Unit =
    continueRequest(context, data, nextFun)
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
    try
      runCleaners(context)
    finally
      control.requestComplete()
  end cleanup


  /** Runs cleaners on the context. */
  private def runCleaners(context: RequestContext[Qos]): Unit =
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
  end runCleaners
end RoutineExecutor
