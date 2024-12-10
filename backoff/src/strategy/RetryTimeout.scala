package io.github.maxkar
package backoff.strategy


/**
 * Backoff/retry timeout management for a single operation.
 *
 * Instances of this trait are stateful and not thread-safe.
 */
trait RetryTimeout {
  /**
   * Returns the timeout before the operation should be retried. This method may
   * be called multiple times and usually returns increasing values on such
   * consequent invocations.
   *
   * @return timeout (milliseconds) before the operation should be retried.
   */
  def getTimeout(): Long
}


object RetryTimeout {
  /**
   * Creates a new operation retry timeout from the given step function. Timeout values
   * produced by the `nextTimeout` function are clipped to the `maxTimeout` value.
   *
   * @param initialTimeout the very first (retry) timeout for an operation.
   * @param nextTimeout function used to derive next timeout from the current
   *   (last used) timeout.
   * @param maxTimeout maximal allowed timeout value.
   * @return a new operation timeout instance with the given properties.
   */
  def fromStepFunction(
        initialTimeout: Long,
        nextTimeout: Long => Long,
        maxTimeout: Long
      ): RetryTimeout =
    new RetryTimeout {
      /** Last returned timeout or -1 if no timeout was returned yet. */
      private var currentTimeout = -1L

      override def getTimeout(): Long = {
        val nextValue =
          if currentTimeout < 0 then
            initialTimeout
          else
            nextTimeout(currentTimeout)

        currentTimeout =
          if nextValue > maxTimeout then maxTimeout else nextValue
        currentTimeout
      }
    }
}
