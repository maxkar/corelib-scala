package io.github.maxkar
package http.server.jetty.qos

import fun.coroutine.Coroutine

import http.server.api.Response

/**
 * An operation (coroutine call) that should be executed by the server runtime
 * and not the application code.
 */
private[qos] abstract sealed class Operation[Qos, T]


object Operation:
  /**
   * The operation that has to be aborted (i.e. finish processing) with
   * the given response.
   * @param response response to send to the client.
   */
  private[qos] case class Abort[Qos, T](response: Response) extends Operation[Qos, T]


  /** Request to read the input up to the given size. */
  private[qos] case class ReadInputBytes[Qos](maxSize: Long) extends Operation[Qos, Array[Byte]]


  /**
   * An operation that is performed on the given request context and
   * consists of modifying request or retrieving data from it.
   */
  private[qos] abstract class ContextOperation[Qos, T] extends Operation[Qos, T]:
    /** Performs the operation upon the given context. */
    def perform(context: RequestContext[Qos]): T
  end ContextOperation


  /**
   * Complex context operation - yields monad instead of the simple value.
   */
  private[qos] abstract class ComplexContextOperation[Qos, T] extends Operation[Qos, T]:
    /**
     * Performs the operation upon the given context but yields
     * another computation instead of plain value.
     */
    def perform(context: RequestContext[Qos]): Coroutine.Routine[({type M[T] = Operation[Qos, T]})#M, T]
  end ComplexContextOperation
end Operation
