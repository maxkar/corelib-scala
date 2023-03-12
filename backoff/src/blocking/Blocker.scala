package io.github.maxkar
package backoff.blocking


/**
 * Implementation of "blocking wait" and timeout management.
 * @param timeoutFn function to build timeout.
 */
final class Blocker(timeoutFn: () => Long):
  /** Synchronization primitive. */
  private val lock = new Object()

  /** Next time when the wakeup should occur. */
  private var deadline: Long = 0

  /** If the timeout was explicitly cancelled. */
  private var cancelled: Boolean = false


  /**
   * Waits for the next time when an attempt should be retried of the timeout is cancelled.
   * @return `true` if timeout was reached or `false` if this timeout was cancelled.
   */
  def waitForRetry(): Boolean =
    var now = System.currentTimeMillis()
    deadline = now + timeoutFn()
    lock synchronized {
      while now < deadline && !cancelled do
        lock.wait(deadline - now)
        now = System.currentTimeMillis()
      end while

      deadline = 0
      return !cancelled
    }
  end waitForRetry


  /**
   * Cancels the timeout. Current `waitForRetry` will be terminated, will yield `false` and all
   * other invocations of `waitForRetry` would terminate immediately with the `false` result.
   */
  def cancel(): Unit =
    lock synchronized {
      cancelled = true
      lock.notifyAll()
    }
  end cancel


  /**
   * Returns time (epoch milliseconds) when the operation should be retried.
   * Returns 0 if there is no timeout is in progress.
   */
  def getNextAttemptTime(): Long =
    lock synchronized {
      deadline
    }

end Blocker
