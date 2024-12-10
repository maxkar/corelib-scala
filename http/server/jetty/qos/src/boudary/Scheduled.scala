package io.github.maxkar
package http.server.jetty.qos.boundary

/**
 * A boundary for scheduling some executions that could be prioritized
 * according to some rules.
 *
 * @tparam M type (constructor) used to encode the execution.
 * @tparam Qos type used to describe "quality of service" (or priority) level.
 */
trait Scheduled[M[_], Qos] {
  /**
   * Schedules the given operation to be executed at some stage later.
   *
   * Exactly one of the callbacks (`onComplete`/`onFailure`) is invoked
   * (and is invoked only once) when the operation completes. The execution
   * **should not** be performed on the thread invoking this method. Clients
   * of this method still should support the case when either callback is
   * invoked before this method returns.
   *
   * @param operation description of the operation to be executed.
   * @param qos quality-of-service defining relative priority of the
   *   operation among other operation.
   * @param ordinal ordinal number of the request that caused the operation.
   *   If two requests have the same Qos class, then requests with a **lower**
   *   ordinal number should have higher priority than the requests with a
   *   higher ordinal.
   * @param onSuccess callback to invoke when the operation is complete.
   * @param onFailure callback to invoke if the operation failed.
   */
  def apply[T](
        operation: M[T],
        qos: Qos,
        ordinal: Long,
        onSuccess: T => Unit,
        onFailure: Throwable => Unit
      ): Unit
}
