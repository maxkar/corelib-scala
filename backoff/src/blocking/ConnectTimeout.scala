package io.github.maxkar
package backoff.blocking

import backoff.strategy

/**
 * Connection timeout (i.e. timeout for "persistent" connections).
 * @param blocker blocking support
 * @param strat connect strategy used by the blocker.
 */
final class ConnectTimeout private(blocker: Blocker, strat: strategy.ConnectTimeout) {
  /**
   * Waits for the next time when an attempt should be retried of the timeout is cancelled.
   * @return `true` if timeout was reached or `false` if this timeout was cancelled.
   */
  def waitForRetry(): Boolean =
    blocker.waitForRetry()


  /**
   * Resets the timeout to its "initial" value. Should be used after connection was
   * successfully established (or the persistent resource was successfully created
   * in some other way).
   */
  def reset(): Unit =
    strat.reset()


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
}

object ConnectTimeout {
  def apply(strat: strategy.ConnectTimeout): ConnectTimeout = {
    val blocker = new Blocker(strat.getTimeout)
    new ConnectTimeout(blocker, strat)
  }
}
