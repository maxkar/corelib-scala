package io.github.maxkar
package sql.database.static

import backoff.blocking.ConnectTimeout
import java.util.concurrent.atomic.AtomicBoolean


/**
 * Database management service. Provides management facilities for the database access service.
 *
 * @param isAlive global "is thread alive" flag.
 * @param threads worker threads and their backoff instances.
 * @param tasks task provider.
 */
final class Service private(
      isAlive: AtomicBoolean,
      threads: Seq[(Thread, ConnectTimeout)],
      tasks: TaskProvider
    ):

  /**
   * Requests that all processing is stopped but does not await until
   * threads are terminated.
   */
  def requestShutdown(): Unit =
    isAlive.set(false)
    tasks.shutdown()
    threads.foreach { t => t._2.cancel() }
  end requestShutdown


  /**
   * Awaits until all the processing is terminated (i.e. until all
   * active requests complete).
   */
  def awaitShutdown(): Unit =
    if isAlive.get() then
      throw new IllegalStateException("Could not await shutdown as it was not requested")
    threads.foreach(_._1.join())
  end awaitShutdown


  /**
   * Requests processing termination and awaits until all the active operations
   * are complete.
   */
  def shutdown(): Unit =
    requestShutdown()
    awaitShutdown()
  end shutdown
end Service


object Service:
  /**
   * Creates and starts a new database access service.
   * @param configuration database access configuration.
   * @param tasks task provider that provides things to execute on the connection.
   * @param sensor sensor for the events arising in this service.
   */
  def apply(
        configuration: Configuration,
        tasks: TaskProvider,
        sensor: Sensor,
      ): Service =

    /* Validate the URL by opening and then closing the connection. */
    if configuration.validation.validateUrl then
      Worker.openConnection(configuration.connection).close()

    val alive = new AtomicBoolean(true)

    val threads =
      Seq.fill(configuration.poolSize) {
        val backoff = ConnectTimeout(configuration.backoffStrategy.newConnectTimeout())
        val worker = new Worker(
          connection = configuration.connection,
          backoffStrategy = backoff,
          validation = configuration.validation,
          taskProvider = tasks,
          sensor = sensor,
          alive = alive
        )

        val thread = configuration.threadFactory.newThread(worker)
        thread.start()
        (thread, backoff)
      }

    new Service(alive, threads, tasks)
  end apply
end Service
