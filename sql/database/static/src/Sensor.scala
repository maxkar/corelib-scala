package io.github.maxkar
package sql.database.static


/**
 * Sensor for various database events. May be called from multiple threads.
 */
trait Sensor:
  /** Invoked when a new connection is about to be created. */
  def createConnectionStarted(): Unit

  /** Invoked after a new DB connection was created successfully. */
  def createConnectionSuccessful(): Unit

  /** Invoked when a new connection could not be created due to an exception. */
  def createConnectionFailed(t: Throwable): Unit


  /** Invoked before a task starts. */
  def taskStarted(): Unit

  /** Invoked when task is executed successfully. */
  def taskSuccess(): Unit

  /** Invoked when task fails. */
  def taskFailed(exn: Throwable): Unit


  /** Invoked before validation starts. */
  def validationStarted(): Unit

  /** Invoked when validation completes. */
  def validationComplete(isValid: Boolean): Unit

  /** Invoked when validation fails due to an exception. */
  def validationFailed(e: Throwable): Unit


  /** Invoked when a general (mysterious) exception occurs. */
  def generalError(e: Throwable): Unit
end Sensor


object Sensor:
  /** No-operation sensor. */
  val noop: Sensor =
    new Sensor {
      override def createConnectionStarted(): Unit = ()
      override def createConnectionSuccessful(): Unit = ()
      override def createConnectionFailed(t: Throwable): Unit = ()

      override def taskStarted(): Unit = ()
      override def taskSuccess(): Unit = ()
      override def taskFailed(exn: Throwable): Unit = ()

      override def validationStarted(): Unit = ()
      override def validationComplete(isValid: Boolean): Unit = ()
      override def validationFailed(e: Throwable): Unit = ()

      override def generalError(e: Throwable): Unit = ()
    }
end Sensor
