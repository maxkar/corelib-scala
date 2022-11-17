package io.github.maxkar
package http.server.jetty.qos

import http.server.api.Response
import http.server.api.Cookie

import javax.servlet.WriteListener
import javax.servlet.ServletOutputStream
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.{Cookie => ServletCookie}


/**
 * An output operation - writing data into the stream.
 * @param module module controlling the operation(s).
 * @param context request context that is being written.
 * @param data data that has to be written.
 */
private final class OutputOperation[Qos](
      module: RoutineExecutor[Qos],
      context: RequestContext[Qos],
      stream: ServletOutputStream,
      data: Array[Byte],
    ) extends WriteListener:

  /** Offset of the next data portion to write. */
  private var offset = 0


  override def onError(t: Throwable): Unit =
    module.outputError(context, t)


  override def onWritePossible(): Unit =
    try
      while stream.isReady() do
        val start = offset

        if start >= data.length then
          stream.close()
          module.finish(context)
          return
        end if

        val chunkSize = Math.min(2048, data.length - start)
        offset = start + chunkSize
        stream.write(data, start, chunkSize)
      end while
    catch
      case e: Throwable => onError(e)
  end onWritePossible
end OutputOperation


/** Output operation - writing request to the context's stream. */
private object OutputOperation:
  /** Outputs response into the given context. */
  def apply[Qos](
        module: RoutineExecutor[Qos],
        context: RequestContext[Qos],
        response: Response,
      ): Unit =
    try
      val servletResponse = context.baseRequest.getResponse()
      servletResponse.setStatus(response.status)

      setHeaders(servletResponse, context.extraHeaders)
      setMultiHeaders(servletResponse, response.headers.entriesIterator)

      setCookies(servletResponse, context.cookies)

      if response.content == null then
        module.finish(context)
      else
        val stream = servletResponse.getOutputStream()
        val handler = new OutputOperation(module, context, stream, response.content)
        stream.setWriteListener(handler)
      end if
    catch
      case e: Throwable =>
        module.outputError(context, e)
  end apply


  /** Sets headers on the response. */
  private def setHeaders(servletResponse: HttpServletResponse, headers: Iterable[(String, String)]): Unit =
    val itr = headers.iterator
    while itr.hasNext do
      val (name, value) = itr.next()
      servletResponse.addHeader(name, value)
    end while
  end setHeaders


  /** Sets "special" headers on the response. */
  private def setMultiHeaders(servletResponse: HttpServletResponse, itr: Iterator[(String, Seq[String])]): Unit =
    while itr.hasNext do
      val (name, values) = itr.next()
      val vitr = values.iterator
      while vitr.hasNext do
        servletResponse.addHeader(name, vitr.next())
    end while
  end setMultiHeaders


  /** Sets cookies on the response. */
  private def setCookies(servletResponse: HttpServletResponse, cookies: Iterable[Cookie]): Unit =
    val itr = cookies.iterator
    while itr.hasNext do
      servletResponse.addCookie(convertCookie(itr.next()))
  end setCookies


  /** Converts cookie into the servlet cookie. */
  private def convertCookie(cookie: Cookie): ServletCookie =
    val res = new ServletCookie(cookie.name, cookie.value)

    cookie.maxAge match
      case None => ()
      case Some(v) => res.setMaxAge(v)

    cookie.path match
      case None => ()
      case Some(value) => res.setPath(value)

    cookie.secure match
      case None => ()
      case Some(value) => res.setSecure(value)

    cookie.httpOnly match
      case None => ()
      case Some(value) => res.setHttpOnly(value)

    res
  end convertCookie
end OutputOperation
