package io.github.maxkar
package http.server.jetty.qos

import fun.coroutine.Coroutine

import http.server.api.Cookie
import http.server.api.Response
import http.server.api.Processing
import http.server.api.ResourceCleaner


/** Implementation of the processing typeclass. */
private final class ProcessingImpl[Qos](
      routine: Coroutine[HQ.Suspension[Qos]]
    )
    extends Processing[HQ.Step[Qos]]:

  override def abort[T](resp: Response): HQ.Step[Qos][T] =
    routine.suspend(Operation.Abort(resp))


  override def addHeaders(headers: (String, String)*): HQ.Step[Qos][Unit] =
    routine.suspend(Effects.AddHeaders(headers))


  override def setCookie(cookie: Cookie): HQ.Step[Qos][Unit] =
    routine.suspend(Effects.AddCookie(cookie))

  override def cleanup(cleaner: => Unit): HQ.Step[Qos][ResourceCleaner[HQ.Step[Qos]]] =
    val c = new Cleaner(() => cleaner)
    val ret = new ResourceCleanerImpl(routine.suspend(Effects.InvokeCleaner(c)))
    routine.suspend(Effects.AddCleaner(c, ret))
  end cleanup


  override def withResource[R](resource: R, cleanup: R => Unit): HQ.Step[Qos][R] =
    val c = new Cleaner(() => cleanup(resource))
    routine.suspend(Effects.AddCleaner(c, resource))
  end withResource


  override def withCleanableResource[R](
        resource: R,
        cleanup: R => Unit,
      ): HQ.Step[Qos][(R, ResourceCleaner[HQ.Step[Qos]])] =
    val c = new Cleaner(() => cleanup(resource))
    val ret = new ResourceCleanerImpl(routine.suspend(Effects.InvokeCleaner(c)))
    routine.suspend(Effects.AddCleaner(c, (resource, ret)))
  end withCleanableResource
end ProcessingImpl
