package io.github.maxkar
package http.server.jetty.qos

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicBoolean


/**
 * "Controller" for processing requests. Mostly coordinates "graceful shutdown"
 * activites across all the requests. This control allows unbounded number of
 * requests in flight.
 *
 * In simple terms, the control just keeps track on the number of requests in
 * flight an allows other parties to coordinate on it.
 */
private[qos] final class RequestControl:
  /**
   * Watermark value - this value is "added" to the number of running
   * requests and should bring the overall "active count" to the negative value.
   * The "negative value" indicates that no more requests should be processed (and
   * the current request (if any) should be discarded).
   */
  private val WATERMARK = -Integer.MAX_VALUE

  /** Synchronization primitive - only for waiting termination. */
  private val terminationLock = new Object()

  /**
   * If this control was already requested to terminate. It is (mostly) used
   * to prevent new stop requests from messing with the main request count.
   */
  private var stopping = new AtomicBoolean(false)

  /**
   * Either number of current requests in flight or (number of requests in flight
   * + WATERMARK). Positive values (no watermark added) mean that processing is active
   * and request should be handled as normal. Negative values (watermark applied) mean
   * that shutdown was requested an no new requests should be accepted.
   */
  private var requestsInFlight = new AtomicInteger()


  /**
   * Checks if the next request should be processed.
   * If this method returns `true`, then `requestComplete` should also be invoked
   * for the request.
   * @return `true` if the request should be processed as usual and `false` if
   *   processing should be aborted as soon as possible.
   */
  def shouldProcessRequest(): Boolean =
    /* Negative value means that stop was requested - don't process the request!. */
    if requestsInFlight.get() < 0 then
      return false

    val newCount = requestsInFlight.incrementAndGet()
    /* Re-check value to avoid race condition(s). */
    if newCount > 0 then
      return true


    return false
  end shouldProcessRequest


  /**
   * Notifies this handler that request was processed. This is mostly
   * used for handling graceful shutdown.
   */
  def requestComplete(): Unit =
    /* Unregister request. Also check if we are terminating AND
     * it was the last running request. If we are not terminating or
     * if there are more requests - do nothing. Otherwise we'll notify
     * waiting nodes.
     */
    if requestsInFlight.decrementAndGet() != WATERMARK then return

    terminationLock synchronized {
      terminationLock.notifyAll()
    }
  end requestComplete


  /**
   * Requests that no more request should be accepted by this control.
   */
  def requestTermination(): Unit =
    /* If set to true, then we are already stopping!. */
    if !stopping.compareAndSet(false, true) then return

    /* Mark the "primary" flag, which is used for checks and notifications.
     * The separate "stopping" is not used as we would like to have some
     * "atomic" checks in the `shouldProcessRequest` and checking two separate
     * fields is not atomic.
     */
    requestsInFlight.addAndGet(WATERMARK)
  end requestTermination


  /** Awaits until there are no more active requests. */
  def awaitTermination(): Unit =
    if !stopping.get() then
      throw new IllegalStateException("Stop was not requested, could not await termination")

    terminationLock synchronized {
      while requestsInFlight.get() != WATERMARK do
        terminationLock.wait()
    }
  end awaitTermination


  /** Terminates processing and awaits until the last request is processed. */
  def terminate(): Unit =
    requestTermination()
    awaitTermination()
  end terminate


  /** Returns a number of requests currently in filght. */
  def getActiveRequestCount(): Int =
    val v = requestsInFlight.get()
    if v >= 0 then v else v - WATERMARK
  end getActiveRequestCount

end RequestControl
