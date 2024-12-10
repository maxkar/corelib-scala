package io.github.maxkar
package sql.database.static

import sql.connection.AutocommitConnection

import scala.collection.mutable.Queue

/**
 * First-in/first-out implementation of the task provider.
 * @tparam T type of the task managed by the queue.
 */
final class FifoTaskProvider extends TaskProvider {
  /** Synchronization primitive. */
  private val lock = new Object()

  /** Indicates if we should shutdown. */
  private var stopped = false

  /** Queue of tasks to execute. */
  private val queue = new Queue[AutocommitConnection => Unit]()


  /** Submits a task for execution. */
  def submit(task: AutocommitConnection => Unit): Unit =
    lock synchronized {
      if (stopped) then
        return
      queue.append(task)
      lock.notify()
    }


  override def getNextTask(timeoutMs: Int): AutocommitConnection => Unit =
    lock synchronized {
      while !stopped && queue.isEmpty do
        lock.wait()
      if stopped then null else queue.dequeue()
    }


  override def shutdown(): Unit =
    lock synchronized {
      if stopped then
        return
      stopped = true
      lock.notifyAll()
    }
}
