package io.github.maxkar
package sql.database.static

import sql.connection.AutocommitConnection

/**
 * A provider of tasks that would be executed on the connection pool.
 */
trait TaskProvider:
  /**
   * Gets a next task to execute on the connection (if any). This method
   * may be called from multiple threads simultaneously.
   *
   * @param timeoutMs maximal timeout to wait for a task.
   * @return a next task to execute. Returns `null` if the timeout has
   *   passed and no tasks are available. Also returns `null` if
   *   `shutdown` was called.
   */
  def getNextTask(timeoutMs: Int): AutocommitConnection => Unit


  /**
   * Indicates that this specific task provider should no longer return any
   * instances and is safe to terminate.
   */
  def shutdown(): Unit
end TaskProvider
