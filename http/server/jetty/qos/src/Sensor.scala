package io.github.maxkar
package http.server.jetty.qos

/** Sensor for the QoS module. */
trait Sensor:
  /**
   * Registers internal error and returns user-visible code that
   * @param requestId server-specific ID of the request.
   * @param error error that happened during request processing.
   * @return error ID that could be returned to end user and that
   *   could later be used by the support team to look-up the issue.
   */
  def internalError(requestId: Long, error: Throwable): String

  /**
   * Registers internal error that could not be seen by end user. For
   * example, resource clean-up issue or input/output error during reading
   * request body will be recorded as such errors.
   *
   * @param requestId server-specific ID of the request.
   * @param error error that happened during request processing.
   */
  def invisibleError(requestId: Long, error: Throwable): Unit =
    internalError(requestId, error)


  /**
   * General error happened in the executor. Quite often - not related
   * to a specific request.
   */
  def genericError(t: Throwable): Unit
end Sensor


object Sensor:
  /** No-operation sensor. Not recommended. */
  object Noop extends Sensor:
    override def internalError(requestId: Long, error: Throwable): String =
      "1GN0RED"

    override def genericError(t: Throwable): Unit = ()
  end Noop

  /**
   * Sensor that just prints the exception. This is somewhat better
   * that the `Noop` but still can't link user-visible codes to
   * the error description.
   */
  object PrintStack extends Sensor:
    override def internalError(requestId: Long, error: Throwable): String =
      error.printStackTrace()
      "REG1STERD-L066ED"

    override def genericError(t: Throwable): Unit =
      t.printStackTrace()
  end PrintStack
end Sensor
