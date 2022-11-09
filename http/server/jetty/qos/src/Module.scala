package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.coroutine.Coroutine

import http.server.api.Cookie
import http.server.api.Response
import http.server.api.Processing
import http.server.api.Route
import http.server.api.ResourceCleaner
import http.server.api.NegotiableErrors

import http.server.toolkit.BaseRoute

import scala.jdk.CollectionConverters.*
import org.eclipse.jetty.http.MetaData.Request

/**
 * Module with quality-of-service support for requests.
 * @tparam Qos User-defineable quality-of-service parameter.
 */
final class Module[Qos] private(
      errors: NegotiableErrors,
      knownMethods: Iterable[String],
      sensor: Module.ErrorSensor,
    ):

  /** Suspension - how the execution could be paused. */
  private type Suspension[T] = Operation[Qos, T]

  /**
   * Type of the evaluation/execution in this module. Typeclass instances
   * like monad or process are defined for this type constructor.
   */
  opaque type Step[T] = Coroutine.Routine[Suspension, T]

  /** Result of the execution. */
  private type StepResult[T] = Coroutine.RunResult[Suspension, T]

  /** Coroutine module with all the typeclasses, etc... */
  private val routine = new Coroutine[Suspension]

  /** Implementation of the monad for the Step. */
  given monadInstance: Monad[Step] = routine.monadInstance


  /** Request processing instance. */
  given processingInstance: Processing[Step] with
    override def abort[T](resp: Response): Step[T] =
      routine.suspend(Operation.Abort(resp))

    override def addHeaders(headers: Seq[(String, String)]): Step[Unit] =
      routine.suspend(Effects.AddHeaders(headers))

    override def setCookie(cookie: Cookie): Step[Unit] =
      routine.suspend(Effects.AddCookie(cookie))

    override def cleanup(cleaner: => Unit): Step[ResourceCleaner[Step]] =
      val c = new Cleaner(() => cleaner)
      val ret = new ResourceCleanerImpl(routine.suspend(Effects.InvokeCleaner(c)))
      routine.suspend(Effects.AddCleaner(c, ret))
    end cleanup


    override def withResource[R](resource: R, cleanup: R => Unit): Step[R] =
      val c = new Cleaner(() => cleanup(resource))
      routine.suspend(Effects.AddCleaner(c, resource))
    end withResource


    override def withCleanableResource[R](
          resource: R,
          cleanup: R => Unit,
        ): Step[(R, ResourceCleaner[Step])] =
      val c = new Cleaner(() => cleanup(resource))
      val ret = new ResourceCleanerImpl(routine.suspend(Effects.InvokeCleaner(c)))
      routine.suspend(Effects.AddCleaner(c, (resource, ret)))
    end withCleanableResource
  end processingInstance


  /** Implementation of the route typeclass for our monad. */
  given routeInstance: Route[Step] = new BaseRoute[Step]:
    override protected def abort[T](response: Response): Step[T] =
      processingInstance.abort(response)

    override protected val errors: NegotiableErrors = Module.this.errors

    override protected val knownMethods: Iterable[String] = Module.this.knownMethods

    override def path[T](fn: PartialFunction[List[String], Step[T]]): Step[T] =
      routine.suspend(new Operation.ComplexContextOperation[Qos, T] {
        override def perform(context: RequestContext[Qos]): Step[T] =
          doRoute(context.initialRequestPath, context.effectivePath, fn)
      })

    override def continue[T](unconsumedPath: List[String], handler: Step[T]): Step[T] =
      routine.suspend(new Operation.ComplexContextOperation[Qos, T] {
        override def perform(context: RequestContext[Qos]): Step[T] =
          context.effectivePath = unconsumedPath
          handler
        end perform
      })


    /** Cached implementation of the "get method" functionality. */
    private val getMethodInstance: Step[String] = routine.suspend(Effects.GetMethod())
    override def getMethod(): Step[String] = getMethodInstance


    /** Cached instance of "get all header names". */
    private val getHeadersInstance: Step[Seq[String]] = routine.suspend(Effects.GetHeaderNames())
    override def getHeaderNames(): Step[Seq[String]] = getHeadersInstance


    override def getHeaders(name: String): Step[Seq[String]] =
      routine.suspend(Effects.GetHeader(name))

    override def getCookies(name: String): Step[Seq[String]] =
      routine.suspend(Effects.GetCookies(name))


    /** Cached instance of "get all parameters names". */
    private val getParametersInstance: Step[Seq[String]] = routine.suspend(Effects.GetParameterNames())
    override def getParameterNames(): Step[Seq[String]] =  getParametersInstance

    override def getParameters(name: String): Step[Seq[String]] =
      routine.suspend(Effects.GetParameter(name))

    override def getBodyAsBytes(limit: Long): Step[Array[Byte]] =
      routine.suspend(Operation.ReadInputBytes(limit))
  end routeInstance


  /** Cached instance of "Get Quality of Service". */
  private val getQosInstance: Step[Qos] = routine.suspend(Effects.GetQos())
  /** Retrieves current value of the "quality of service" parameter. */
  def getQos(): Step[Qos] = getQosInstance

  /**
   * Adjusts the (program-defined) quality of service for the current request.
   * @param newQos new "quality-of-service" parameter that affects how request
   *   processing is being scheduled.
   */
  def setQos(newQos: Qos): Step[Unit] =
    routine.suspend(Effects.SetQos(newQos))


  /**
   * Continues processing of the given monad according to all the defined rules.
   * This method may be called from **any** thread.
   */
  private def continueRequest(context: RequestContext[Qos]): Unit = ???


  /** Performs (asynchronous) input for the given request. */
  private def doInput(context: RequestContext[Qos], next: Array[Byte] => Step[Response]): Unit =
    val baos = new java.io.ByteArrayOutputStream()
    val buf = new Array[Byte](4096)
    val is = context.baseRequest.getInputStream()
    var finished = false

    is.setReadListener(new javax.servlet.ReadListener() {
      override def onAllDataRead(): Unit =
        baos.close()

        if finished then return
        finished = true

        try
          context.nextSteps = next(baos.toByteArray())
          continueRequest(context)
        catch
          case e: Throwable => raiseInternalError(context, e)
      end onAllDataRead


      override def onDataAvailable(): Unit =
        try
          while is.isReady() do
            val read = is.read(buf)
            if (read < 0) then
              onAllDataRead()
            else
              baos.write(buf, 0, read)
        catch
          case e: Throwable => raiseInternalError(context, e)
      end onDataAvailable


      override def onError(t: Throwable): Unit =
        raiseInternalError(context, t)
      end onError
    })
  end doInput



  /**
   * Raises an internal error and aborts request processing.
   * This may be called from **any** thread.
   */
  private def raiseInternalError(context: RequestContext[Qos], e: Throwable): Unit =
    /* First, try to generate response.
     * Everything may fail so we are trying to be extra careful. */
    val errResponse =
      try
        val ref = sensor.internalError(context.serial, e)
        errors.internalError(context.baseRequest.getHeaders("Accept").asScala.toSeq, ref)
      catch
        case e: Throwable =>
          try
            sensor.invisibleError(context.serial, e)
          catch
            case e: Throwable => ()
          end try
          Response(500)()
      end try

    /* Now, complete the request. Depending on where it happened it may be either
     * Jetty thread (input routine) or Internal Thread (raised as part of the
     * coroutine evaluation). We handle both failures on the same thread where
     * it happened.
     */
    completeWithResponse(context, errResponse)
  end raiseInternalError


  /** Completes the request by sending the response to it. */
  private def completeWithResponse(context: RequestContext[Qos], resp: Response): Unit =
    try
      var r = context.baseRequest.getResponse()
      r.setStatus(resp.status)

      val headers1 = context.extraHeaders.iterator
      while headers1.hasNext do
        val (header, value) = headers1.next()
        r.addHeader(header, value)
      end while

      val headers2 = resp.headers.entriesIterator
      while headers2.hasNext do
        val (header, values) = headers2.next()
        val viter = values.iterator
        while viter.hasNext do
          r.addHeader(header, viter.next())
      end while

      val cookies = context.cookies.iterator
      while cookies.hasNext do
        val cookie = cookies.next()
        val c = new javax.servlet.http.Cookie(cookie.name, cookie.value)
        cookie.maxAge match
          case None => ()
          case Some(v) => c.setMaxAge(v)
        cookie.path match
          case None => c
          case Some(value) => c.setPath(value)
        cookie.secure match
          case None => ()
          case Some(value) => c.setSecure(value)
        cookie.httpOnly match
          case None => ()
          case Some(value) => c.setHttpOnly(value)
        r.addCookie(c)
      end while

      /** Mark as complete. */
      if resp.content == null then
        finish(context)
      else
        writeBytesAsync(context, resp.content)

    catch
      /* If nothing works - we log the error and just mark the request as complete. */
      case e: Throwable =>
        try
          sensor.invisibleError(context.serial, e)
        catch
          case e: Throwable => ()
        end try
        finish(context)
    end try
  end completeWithResponse


  /**
   * Performs asynchronous output of the buffer. Why asynchronous?
   * Because writing 2GB of data does not seem to be non-blocking operation
   * and Servlet spec does not discuss data ownership so writing large
   * chunks is expected to block (at least sometimes).
   */
  private def writeBytesAsync(context: RequestContext[Qos], bytes: Array[Byte]): Unit =
    var ptr = 0
    val stream = context.baseRequest.getResponse().getOutputStream()

    try
      stream.setWriteListener(new javax.servlet.WriteListener {
        override def onError(t: Throwable): Unit =
          try
            sensor.invisibleError(context.serial, t)
          finally
            finish(context)
          end try
        end onError


        override def onWritePossible(): Unit =
          try
            while stream.isReady() do
              val start = ptr
              if start >= bytes.length then
                finish(context)
                return
              val toWrite = Math.min(bytes.length - start, 2048)
              ptr = start + toWrite
              stream.write(bytes, start, toWrite)
            end while
          catch
            case e: Throwable => onError(e)
          end try
        end onWritePossible
      })
    catch
      /* If nothing works - we log the error and just mark the request as complete. */
      case e: Throwable =>
        try
          sensor.invisibleError(context.serial, e)
        catch
          case e: Throwable => ()
        end try
        finish(context)
    end try
  end writeBytesAsync


  /**
   * "Finishes" processing the context by notifying servlet about processing being
   * complete and runs the cleaners associated with the request.
   */
  private def finish(context: RequestContext[Qos]): Unit =
    try
      context.baseRequest.getAsyncContext().complete()
    finally
      cleanup(context)
  end finish



  /**
   * Cleans-up the context by running all the registered cleaners.
   */
  private def cleanup(context: RequestContext[Qos]): Unit =
    var cleaner = context.cleaner
    context.cleaner = null
    while cleaner != null do
      val nextCleaner = cleaner.next
      try
        cleaner.drop()
        cleaner.performCleanup()
      catch
        case e: Throwable =>
          try
            sensor.invisibleError(context.serial, e)
          catch
            case e: Throwable =>
              /* Explicitly ignore. We don't want to cause resource leak if
               * there are some sensor issues. Or at least we should make a best
               * effort to cleanup those resources.
               */
              ()
          end try
      end try
      cleaner = nextCleaner
    end while
  end cleanup


end Module


object Module:
  /** Sensor for internal errors. */
  trait ErrorSensor:
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
  end ErrorSensor


  object ErrorSensor:
    /** No-operation sensor. Not recommended. */
    object Noop extends ErrorSensor:
      override def internalError(requestId: Long, error: Throwable): String =
        "1GN0RED"
    end Noop

    /**
     * Sensor that just prints the exception. This is somewhat better
     * that the `Noop` but still can't link user-visible codes to
     * the error description.
     */
    object PrintStack extends ErrorSensor:
      override def internalError(requestId: Long, error: Throwable): String =
        error.printStackTrace()
        "REG1STERD-L066ED"
    end PrintStack
  end ErrorSensor

  /**
   * Creates a new module with Quality-of-service support.
   * @tparam Qos user-driven way to describe quality of service.
   * @param errors error handlers used by this server.
   * @param knownMethods server-wide HTTP methods (the ones that
   *   may be reasonably expected from any handler).
   */
  def apply[Qos: Ordering](
        errors: NegotiableErrors,
        knownMethods: Iterable[String] = Route.defaultMethod,
        sensor: ErrorSensor,
      ): Module[Qos] =
    new Module(errors, knownMethods, sensor)
end Module
