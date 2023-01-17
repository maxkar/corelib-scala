package io.github.maxkar
package backoff


/**
 * A backoff/timeout management for a persistent connection to a
 * particular resource. Instances of this trait may be used to control
 * Database and Messag Queue connections, (persistent) TCP connection to
 * another service or any other similar thing. The resource in question
 * does not have to be a "network" connection. ConnectionTimeout instances
 * may be used to control retry strategies used in accessing named
 * pipes or shared memory regions. What is common in all the scenarios
 * is that the "connectivity" is considered to be persistent - it should
 * exist for some continuous period of time and when communication error
 * occurs the (logical) connection should be reestablished.
 *
 * Instances of this trait are not thread-safe.
 */
trait ConnectTimeout:
  /**
   * Returns the timeout before the connection should be established for a next time.
   * This operation may be called multiple times in a row and usually returns
   * increalising values.
   *
   * @return timeout (milliseconds) before the next (re)connection attempt should be made.
   */
  def getTimeout(): Long


  /**
   * "Resets" the timeout to initial values after the connection was successfully established.
   * This usually mean that the timeout is reset to its initial value. However specific
   * implementations may use other (time-based) strategies on how to use the timeout(s).
   */
  def reset(): Unit
end ConnectTimeout


object ConnectTimeout:
  /**
   * Creates a new connection timeout from the given step function. Timeout values
   * produced by the `nextTimeout` function are clipped to the `maxTimeout` value.
   * Resetting values sets the timeout to its initial value.
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
      ): ConnectTimeout =
    new ConnectTimeout:
      /**
       * Last returned retry timeout in the "failure" chain.
       * Set to -1 if no calls to `getTimeout` was made (no connection attempts was made)
       * or `reset` was called (a successfull connection was made).
       */
      private var currentTimeout = -1L

      override def getTimeout(): Long =
        val nextValue =
          if currentTimeout < 0 then
            initialTimeout
          else
            nextTimeout(currentTimeout)

        currentTimeout =
          if nextValue > maxTimeout then maxTimeout else nextValue
        currentTimeout
      end getTimeout

      override def reset(): Unit =
        currentTimeout = -1
    end new
end ConnectTimeout
