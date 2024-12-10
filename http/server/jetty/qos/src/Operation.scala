package io.github.maxkar
package http.server.jetty.qos

import fun.coroutine.Coroutine

import http.server.api.Response

/**
 * An operation (coroutine call) that should be executed by the server runtime
 * and not the application code.
 */
private abstract sealed class Operation[Qos, T]


private object Operation {
  /**
   * The operation that has to be aborted (i.e. finish processing) with
   * the given response.
   * @param response response to send to the client.
   */
  private[qos] case class Abort[Qos, T](response: Response) extends Operation[Qos, T]


  /** Request to read the input up to the given size. */
  private[qos] case class ReadInputBytes[Qos](maxSize: Long) extends Operation[Qos, Array[Byte]]


  /** Sets QoS value. */
  private[qos] case class SetQos[Qos](qos: Qos) extends Operation[Qos, Unit]


  /** Performs a completable operation. */
  private[qos] case class RunCompletable[Qos, S[_], T](
        operation: S[T],
        tc: boundary.Completable[S])
      extends Operation[Qos, T]


  /** Performs a scheduled operation. */
  private[qos] case class RunScheduled[Qos, S[_], T](
        operation: S[T],
        tc: boundary.Scheduled[S, Qos])
      extends Operation[Qos, T]


  /**
   * Raises an internal exception for the request. Somewhat similar to abort but
   * takes an exception. Used by boundaries where exception formatting and response
   * generation should be moved from external (another module's) thread pool into the
   * web pool.
   */
  private[qos] case class Raise[Qos, T](error: Throwable) extends Operation[Qos, T]


  /**
   * An operation that is performed on the given request context and
   * consists of modifying request or retrieving data from it.
   */
  private[qos] abstract class ContextOperation[Qos, T] extends Operation[Qos, T] {
    /** Performs the operation upon the given context. */
    def perform(context: RequestContext[Qos]): T
  }


  /**
   * Complex context operation - yields monad instead of the simple value.
   */
  private[qos] abstract class ComplexContextOperation[Qos, T] extends Operation[Qos, T] {
    /**
     * Performs the operation upon the given context but yields
     * another computation instead of plain value.
     */
    def perform(context: RequestContext[Qos]): Coroutine.Routine[({type M[T] = Operation[Qos, T]})#M, T]
  }
}
