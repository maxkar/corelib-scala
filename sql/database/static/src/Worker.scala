package io.github.maxkar
package sql.database.static

import java.sql.Driver
import java.sql.DriverManager
import java.sql.{Connection => JdbcConnection}

import sql.connection.AutocommitConnection

import java.util.concurrent.atomic.AtomicBoolean


/**
 * A database worker (worker thread).
 *
 * @param connection connection options.
 * @param backoffStrategy back-off strategy instance for use on connection attempts.
 * @param validation connection validation configuration.
 * @param taskProvider provider of the tasks to execute on the worker.
 * @param sensor sensor for various events that happen during connection lifecycle.
 * @param alive indicates that we are "alive" - should try and work. If set to false,
 *   the worker have to terminate.
 */
private final class Worker(
      connection: Configuration.Connection,
      backoffStrategy: backoff.Backoff,
      validation: Configuration.Validation,
      taskProvider: TaskProvider,
      sensor: Sensor,
      alive: AtomicBoolean,
    ) extends Runnable:


  override def run(): Unit =
    var conn = connect()

    while conn != null && alive.get() do
      try
        serveUsing(conn)
      catch
        case e: Throwable => sensor.generalError(e)
      finally
        try
          conn.close()
        catch
          case e: Throwable => sensor.generalError(e)
        end try
      end try

      conn = connect()
    end while
  end run


  /**
   * Attempts to get a connection using connection properties and back-off strategy.
   * Returns a connection or `null` if termination condition was reached.
   */
  private def connect(): JdbcConnection =
    while alive.get() do
      sensor.createConnectionStarted()
      try
        val res = Worker.openConnection(connection)
        sensor.createConnectionSuccessful()
        backoffStrategy.reset()
        return res
      catch
        case e: Throwable =>
          sensor.createConnectionFailed(e)
          backoffStrategy.handleFailure()
      end try
    end while

    return null
  end connect


  /**
   * Runs the process using the active connection. Returns at the moment
   * connection is indicated to be invalid.
   */
  private def serveUsing(conn: JdbcConnection): Unit =
    while alive.get() do
      val task = taskProvider.getNextTask(validation.validateOnIdleMsTimeout)
      /* If the task is not null, then there is no timeout and we should execute
       * the task. Otherwise it was either shutdown or timeout before validation,
       * in both these cases it is safe to call validate to handle both cases.
       *
       * In both cases if validation fails then return from this method as connection
       * should be re-established.
       */
      if task != null then
        /*
         * Only run validation if task failed. Otherwise assume that task used the connection
         * successfully and it is valid.
         */
        if !runTask(conn, task) && !validate(conn) then
          return
      else
        if !validate(conn) then
          return
      end if
    end while
  end serveUsing


  /**
   * Runs one task on the connection.
   * @return `true` if task completed successfully and `false` otherwise.
   */
  private def runTask(conn: JdbcConnection, task: AutocommitConnection => Unit): Boolean =
    sensor.taskStarted()
    try
      task(new AutocommitConnection(conn))
      sensor.taskSuccess()
      true
    catch
      case e: Throwable =>
        sensor.taskFailed(e)
        false
    end try
  end runTask


  /** Checks if the existing connection is valid and could be used. */
  private def validate(conn: JdbcConnection): Boolean =
    if !alive.get() then
      return false
    sensor.validationStarted()
    try
      val res = conn.isValid(validation.validationTimeoutMs)
      sensor.validationComplete(res)
      res
    catch
      case e: Throwable =>
        sensor.validationFailed(e)
        false
    end try
  end validate
end Worker



private object Worker:
  /** Opens a new JDBC connection to the database. */
  def openConnection(connection: Configuration.Connection): JdbcConnection =
    connection match
      case Configuration.Connection.NoCredentials(url) =>
        DriverManager.getConnection(url)
      case Configuration.Connection.LoginPassword(url, login, password) =>
        DriverManager.getConnection(url, login, password)
      case Configuration.Connection.PropertyBased(url, properties) =>
        DriverManager.getConnection(url, properties)
    end match
end Worker
