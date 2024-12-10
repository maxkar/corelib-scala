package io.github.maxkar
package http.server.jetty.qos.boundary

/**
 * A "completable" execution - something that could be listened to and be notified
 * when the main execution completes.
 *
 * @tparam M type constructor used to represent execution/computation.
 */
trait Completable[M[_]] {
  /**
   * Registers callbacks and executes them when value of the `base` is available.
   *
   * Exactly one of the callbacks (either `success` or `failure`) is invoked (and is
   * invoked exactly once) when the execution completes. The callback may be invoked
   * on **any** thread, including the thread calling `onComplete` (usually when result
   * is already available when this method is called).
   *
   * @param computation computation that is being listened to.
   * @param onSuccess callback to invoke when execution completes successfully.
   * @param onFailure callback to invoke when execution completes abruptly.
   */
  def onComplete[T](
        computation: M[T],
        onSuccess: T => Unit,
        onFailure: Throwable => Unit,
      ): Unit
}
