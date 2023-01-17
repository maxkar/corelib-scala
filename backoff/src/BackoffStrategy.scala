package io.github.maxkar
package backoff

/**
 * A general configuration of how backing-off (timeout management) should be
 * performed in the application. Instances of this trait capture initial timeout
 * values and how these values should evolve over time.
 *
 * The strategy is intended to be generic enough to be able to create both
 * simple RetryTimeouts (for individual operations) and ConnectionTimeouts
 * (for persistent connections).
 */
trait BackoffStrategy:
  /** Creates a new instance of the retry timeout. */
  def newRetryTimeout(): RetryTimeout

  /** Creates a new instance of the (persistent) connection timeout. */
  def newConnectTimeout(): ConnectTimeout
end BackoffStrategy


object BackoffStrategy:
  /**
   * Creates a new strategy that uses "step function" to derive consequent timeouts.
   * Timeout values produced by the `nextTimeout` function are clipped to the
   * `maxTimeout` value. Resetting values sets the timeout to its initial value.
   *
   * @param initialTimeout the very first (retry) timeout for an operation.
   * @param nextTimeout function used to derive next timeout from the current
   *   (last used) timeout. The function must be stateless as it will be reused across
   *   multiple individual timeout instances.
   * @param maxTimeout maximal allowed timeout value.
   * @return a new operation timeout instance with the given properties.
   */
  def fromStepFunction(
        initialTimeout: Long,
        nextTimeout: Long => Long,
        maxTimeout: Long
      ): BackoffStrategy =
    new BackoffStrategy:
      override def newConnectTimeout(): ConnectTimeout =
        ConnectTimeout.fromStepFunction(initialTimeout, nextTimeout, maxTimeout)

      override def newRetryTimeout(): RetryTimeout =
        RetryTimeout.fromStepFunction(initialTimeout, nextTimeout, maxTimeout)
    end new

end BackoffStrategy
