package io.github.maxkar
package http.server.api


/** Request-processing functionality apart the one included in Route. */
trait Processing[M[_]] {
  /**
   * Aborts processing of the current request, sends the `response` to the
   * client and releases all the resources associated with the current process.
   *
   * This method represents non-local control transfer that could not be
   * interrupted or caught by the client code.
   *
   * @param response response to send to the client.
   */
  def abort[T](response: Response):M[T]


  /**
   * Sets a cookie to be sent to the client when request is complete.
   *
   * The cookie would be sent if request was completed naturally or aborted.
   * It may be not sent if handler raised an exception during processing.
   *
   */
  def setCookie(cookie: Cookie): M[Unit]


  /**
   * Adds headers that would be added after the request was processed
   * (either natural completion or call to `abort`). These headers are not guaranteed
   * to be set if the handler raises an exception.
   *
   * The most common use cases are setting CORS headers and renewing authentication tokens.
   */
  def addHeaders(headers: (String, String)*): M[Unit]


  /**
   * Registers a clean-up routine for some resource. The clean-up
   * routine will be invoked exactly once when the first of the following
   * events happens:
   *  * The `ResourceCleaner.clean()` method is explicitly invoked during the request processing.
   *  * After response was sent to the client.
   *  * After an error occured during communication with the client.
   *
   * The callbacks registered with the cleaner are invoked in the order reverse to the order in
   * which they were registered.
   *
   * The cleanup method is a natuaral way to manage pooled resources like memory buffers.
   */
  def cleanup(cleaner: => Unit): M[ResourceCleaner[M]]

  /**
   * Registers resource cleanup and returns the initial resource.
   * This is similar to the `cleanup` but with syntax that suits other scenarios:
   *
   * ```
   *   for
   *     resource <- process.withResource(pool.get(), pool.ret)
   *     ...
   * ```
   */
  def withResource[R](resource: R, cleanup: R => Unit): M[R]


  /**
   * Registers resource and returns the manual clean-up utility. Similar to `withResource` but
   * also returns the manual cleaner. May be used were resource lifetime is limited but processing
   * may still abort due to some other reason:
   *
   * ```
   * for
   *   (buffer, cleaner) <- process.withCleanableResource(pool.get(), pool.ret)
   *   data <- parseUsingBuffer(buffer)
   *   _ <- cleaner.clean()
   * yield ...
   * ```
   *
   * The code above allocate temporary buffer for parsing. It is known that the buffer won't be
   * needed after the parsing is complete (and thus it is manually returned). However the parsing
   * may encounter an error and call the `abort` method. The cleanup method still will be invoked
   * in this case despite the fact the `cleaner.clean()` line is never reached.
   */
  def withCleanableResource[R](resource: R, cleanup: R => Unit): M[(R, ResourceCleaner[M])]
}

object Processing {
  /**
   * Aborts processing of the current request, sends the `response` to the
   * client and releases all the resources associated with the current process.
   *
   * This method represents non-local control transfer that could not be
   * interrupted or caught by the client code.
   *
   * @param response response to send to the client.
   */
  inline def abort[M[_], T](response: Response)(using p: Processing[M]):M[T] =
    p.abort(response)


  /**
   * Sets a cookie to be sent to the client when request is complete.
   *
   * The cookie would be sent if request was completed naturally or aborted.
   * It may be not sent if handler raised an exception during processing.
   *
   * @param name name of the cookie to set.
   * @param value value of the cookie to set.
   * @param maxAge max cookie validity time.
   * @param path path for which the cookie will be applied.
   * @param secure if the cookie is only available over HTTPs (or other secure channel).
   * @param httpOnly set to true if cookie should be present in the
   *   HTTP exchange with the server but not available to JavaScript.
   */
  def setCookie[M[_]](
        name: String,
        value: String,
        maxAge: Option[Int] = None,
        path: Option[String] = None,
        secure: Option[Boolean] = None,
        httpOnly: Option[Boolean] = None,
      )(
        using p: Processing[M]
      ): M[Unit] =
    p.setCookie(Cookie(name, value, maxAge, path, secure, httpOnly))


  /**
   * Sets a cookie to be sent to the client when request is complete.
   *
   * The cookie would be sent if request was completed naturally or aborted.
   * It may be not sent if handler raised an exception during processing.
   *
   * @param cookie cookie to set.
   */
  inline def setCookie[M[_]](cookie: Cookie)(using p: Processing[M]): M[Unit] =
    p.setCookie(cookie)


  /**
   * Adds headers that would be added after the request was processed
   * (either natural completion or call to `abort`). These headers are not guaranteed
   * to be set if the handler raises an exception.
   *
   * The most common use cases are setting CORS headers and renewing authentication tokens.
   */
  inline def addHeaders[M[_]](
        headers: (String, String)*,
      )(using
        p: Processing[M]
      ): M[Unit] =
    p.addHeaders(headers*)


  /**
   * Registers a clean-up routine for some resource. The clean-up
   * routine will be invoked exactly once when the first of the following
   * events happens:
   *  * The `ResourceCleaner.clean()` method is explicitly invoked during the request processing.
   *  * After response was sent to the client.
   *  * After an error occured during communication with the client.
   *
   * The callbacks registered with the cleaner are invoked in the order reverse to the order in
   * which they were registered.
   *
   * The cleanup method is a natuaral way to manage pooled resources like memory buffers.
   */
  inline def cleanup[M[_], T](
        cleaner: => Unit,
      )(using
        p: Processing[M],
      ): M[ResourceCleaner[M]] =
    p.cleanup(cleaner)


  /**
   * Registers resource cleanup and returns the initial resource.
   * This is similar to the `cleanup` but with syntax that suits other scenarios:
   *
   * ```
   *   for
   *     resource <- Process.withResource(pool.get(), pool.ret)
   *     ...
   * ```
   */
  inline def withResource[M[_], R](
        resource: R,
        cleanup: R => Unit,
      )(using
        p: Processing[M]
      ): M[R] =
    p.withResource(resource, cleanup)


  /**
   * Registers resource and returns the manual clean-up utility. Similar to `withResource` but
   * also returns the manual cleaner. May be used were resource lifetime is limited but processing
   * may still abort due to some other reason:
   *
   * ```
   * for
   *   (buffer, cleaner) <- process.withCleanableResource(pool.get(), pool.ret)
   *   data <- parseUsingBuffer(buffer)
   *   _ <- cleaner.clean()
   * yield ...
   * ```
   *
   * The code above allocate temporary buffer for parsing. It is known that the buffer won't be
   * needed after the parsing is complete (and thus it is manually returned). However the parsing
   * may encounter an error and call the `abort` method. The cleanup method still will be invoked
   * in this case despite the fact the `cleaner.clean()` line is never reached.
   */
  inline def withCleanableResource[M[_], R](
        resource: R,
        cleanup: R => Unit,
      )(using
        p: Processing[M]
      ): M[(R, ResourceCleaner[M])] =
    p.withCleanableResource(resource, cleanup)
}
