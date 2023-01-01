package io.github.maxkar
package sql.database.static.backoff


/**
 * Back-off strategy management. Indicates how to handle timeouts
 * between (unsuccessfull) operations.
 *
 * The `reset` and `handleFailure` methods are invoked from a single
 * thread only. The `abortSleep` method may be called from multiple
 * threads and should be safe in regard with all other methods.
 */
trait Backoff:
  /** Resets the back-off interval after a successfull operation. */
  def reset(): Unit


  /**
   * Handles a failure by awaiting for some backoff-specific time. Consequent
   * calls to this method may change the timeout being used.
   */
  def handleFailure(): Unit


  /**
   * Instructs the back-off to finish any waits/sleeps in progress,
   * return from the active `handleFailure` methods and never sleep again.
   * This method is used when a whole subsystem is shutdown and any pending
   * back-offs should also be terminated.
   */
  def shutdown(): Unit
end Backoff
