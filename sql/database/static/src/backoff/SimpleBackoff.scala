package io.github.maxkar
package sql.database.static.backoff

/**
 * A simple back-off strategy starting with a fixed interval and then
 * using the provided function to calculate any further backoffs.
 *
 * @param initialBackoffMs initial value of the back-off. Used on a first
 *   attempt to set the back-off.
 * @param  maxBackoffMs maximal back-off timeout, timeouts after this value
 *   would be clipped to it.
 * @param nextTimeout function used to derive next timeout from the current one.
 */
final class SimpleBackoff(initialBackoffMs: Int, maxBackoffMs: Int, nextTimeout: Int => Int)
    extends Backoff:

  /** Current back-off timeout, used on the next operation. */
  private var currentBackoffMs: Int = initialBackoffMs

  /** Indicates that the backoff was shut down. */
  private var wasShutDown: Boolean = false

  /** Incapsulated synchronization primitive. */
  private val lock = new Object()


  override def handleFailure(): Unit =
    lock synchronized {
      if wasShutDown then
        return
      doSleep(currentBackoffMs)
    }
    if !wasShutDown && currentBackoffMs < maxBackoffMs then
      currentBackoffMs = Math.min(nextTimeout(currentBackoffMs), maxBackoffMs)
  end handleFailure


  override def reset(): Unit =
    currentBackoffMs = initialBackoffMs


  override def shutdown(): Unit =
    lock synchronized {
      wasShutDown = true
      lock.notifyAll()
    }


  /** Performs "notifiable" sleep. */
  private def doSleep(timeout: Int): Unit =
    var now = System.currentTimeMillis()
    val deadline = now + timeout
    /* Handle spurious wakeups. */
    while now < deadline do
      lock.wait(deadline - now)
      /* But not all wakeups are suprious, we may have been notified. */
      if wasShutDown then
        return
      now = System.currentTimeMillis()
    end while
  end doSleep


end SimpleBackoff
