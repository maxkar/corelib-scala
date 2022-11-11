package io.github.maxkar
package http.server.jetty.qos

import java.io.ByteArrayOutputStream

import javax.servlet.ReadListener
import javax.servlet.ServletInputStream

import http.server.api.Response

/**
 * Implementation of the input operation.
 * @param module module that would be used to handle context lifecycle events.
 * @param context context in which the operation is performed.
 * @param stream stream that has to be used for reading.
 * @param limit IO limit (how much bytes could be read before aborting with the
 *   "request entity too large" response).
 * @param nextSteps next steps to perform on the given request.
 */
private class InputOperation[QoS](
      module: RoutineExecutor[QoS],
      context: RequestContext[QoS],
      stream: ServletInputStream,
      limit: Long,
      nextSteps: Array[Byte] => HQ.Step[QoS][Response]
    ) extends ReadListener:
  /** Collector for the request data. */
  private var baos = new ByteArrayOutputStream()

  /** Temporary buffer to use in IO operations. */
  private var buf = new Array[Byte](4096)

  /** How much bytes are remaining to read. */
  private var remaining = limit


  /**
   * Finished - indicates we already finished processing in
   * "some" way (theoretically, we may see "endOfFile" during data read
   * and we would like to prevent this from happening).
   */
  private var finished = false


  override def onAllDataRead(): Unit =
    if finished then return
    finished = true

    try
      stream.close()
      baos.close()
      val data = baos.toByteArray()
      module.completeInput(context, data, nextSteps)
    catch
      case e: Throwable =>
        module.raiseInternalError(context, e)
  end onAllDataRead


  override def onError(t: Throwable): Unit =
    finished = true
    module.raiseInternalError(context, t)
  end onError


  override def onDataAvailable(): Unit =
    try
      while stream.isReady() do
        val rd = stream.read(buf)
        /* Unlikely to happen, but Servlet spec is not very clear
         * if EOF could be reported in this case.
         */
        if rd < 0 then
          onAllDataRead()
          return

        remaining -= rd
        if remaining < 0 then
          module.raiseRequestSizeTooLarge(context, limit)
          return

        baos.write(buf, 0, rd)
      end while
    catch
      case e: Throwable => module.raiseInternalError(context, e)
    end try

end InputOperation


/**
 * Input operation - reading servlet input stream and eventually
 * returning the result.
 */
private[qos] object InputOperation:
  /**
   * Starts a new input operation for the given context.
   * @param module module that would be used to handle context lifecycle events.
   * @param context context in which the operation is performed.
   * @param stream stream that has to be used for reading.
   * @param limit IO limit (how much bytes could be read before aborting with the
   *   "request entity too large" response).
   * @param nextSteps next steps to perform on the given request.
   */
  def apply[Qos](
        module: RoutineExecutor[Qos],
        context: RequestContext[Qos],
        limit: Long,
        nextSteps: Array[Byte] => HQ.Step[Qos][Response],
      ): Unit =
    val stream = context.baseRequest.getInputStream()
    val handler = new InputOperation(module, context, stream, limit, nextSteps)
    stream.setReadListener(handler)
  end apply
end InputOperation
