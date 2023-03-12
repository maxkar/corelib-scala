package io.github.maxkar
package backoff.blocking

import backoff.strategy

/**
 * Blocking retry timeout.
 * @param blocker blocking support.
 */
final class RetryTimeout private(blocker: Blocker):
  /**
   * Waits for the next time when an attempt should be retried of the timeout is cancelled.
   * @return `true` if timeout was reached or `false` if this timeout was cancelled.
   */
  def waitForRetry(): Boolean =
    blocker.waitForRetry()


  /**
   * Cancels the timeout. Current `waitForRetry` will be terminated, will yield `false` and all
   * other invocations of `waitForRetry` would terminate immediately with the `false` result.
   */
  def cancel(): Unit =
    blocker.cancel()


  /**
   * Returns time (epoch milliseconds) when the operation should be retried.
   * Returns 0 if there is no timeout is in progress.
   */
  def getNextAttemptTime(): Long =
    blocker.getNextAttemptTime()
end RetryTimeout



object RetryTimeout:
  /** Creates a new retry timeout with the given timeout strategy. */
  def apply(strat: strategy.RetryTimeout): RetryTimeout =
    val blocker = new Blocker(strat.getTimeout)
    new RetryTimeout(blocker)
  end apply
end RetryTimeout
