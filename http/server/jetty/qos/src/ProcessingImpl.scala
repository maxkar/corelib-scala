package io.github.maxkar
package http.server.jetty.qos

import fun.coroutine.Coroutine

import http.server.api.Cookie
import http.server.api.Response
import http.server.api.Processing
import http.server.api.ResourceCleaner


/** Implementation of the processing typeclass. */
private final class ProcessingImpl[Qos]()
    extends Processing[HQ.Step[Qos]] {

  override def abort[T](resp: Response): HQ.Step[Qos][T] =
    Coroutine.call(Operation.Abort(resp))


  override def addHeaders(headers: (String, String)*): HQ.Step[Qos][Unit] =
    Coroutine.call(Effects.AddHeaders(headers))


  override def setCookie(cookie: Cookie): HQ.Step[Qos][Unit] =
    Coroutine.call(Effects.AddCookie(cookie))

  override def cleanup(cleaner: => Unit): HQ.Step[Qos][ResourceCleaner[HQ.Step[Qos]]] = {
    val c = new Cleaner(() => cleaner)
    val ret = new ResourceCleanerImpl[HQ.Step[Qos]](Coroutine.call(Effects.InvokeCleaner(c)))
    Coroutine.call(Effects.AddCleaner(c, ret))
  }


  override def withResource[R](resource: R, cleanup: R => Unit): HQ.Step[Qos][R] = {
    val c = new Cleaner(() => cleanup(resource))
    Coroutine.call(Effects.AddCleaner(c, resource))
  }


  override def withCleanableResource[R](
        resource: R,
        cleanup: R => Unit,
      ): HQ.Step[Qos][(R, ResourceCleaner[HQ.Step[Qos]])] = {
    val c = new Cleaner(() => cleanup(resource))
    val ret = new ResourceCleanerImpl[HQ.Step[Qos]](Coroutine.call(Effects.InvokeCleaner(c)))
    Coroutine.call(Effects.AddCleaner(c, (resource, ret)))
  }
}
