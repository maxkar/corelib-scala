package io.github.maxkar
package http.server.jetty.qos

import fun.typeclass.Monad
import fun.coroutine.Coroutine

import http.server.api.Cookie
import http.server.api.Response
import http.server.api.Processing
import http.server.api.ResourceCleaner


/**
 * Module with quality-of-service support for requests.
 * @tparam Qos User-defineable quality-of-service parameter.
 */
final class Module[Qos]:
  /**
   * Type of the evaluation/execution in this module. Typeclass instances
   * like monad or process are defined for this type constructor.
   */
  opaque type Step[T] = Coroutine.Routine[Suspension, T]

  /** Result of the execution. */
  private type StepResult[T] = Coroutine.RunResult[Suspension, T]


  /** How to suspend execution and what to do next. */
  private enum Suspension[T]:
    /** Abort execution and do nothing else. */
    case Abort(resp: Response)

    /** Adds headers to the resulting request. */
    case AddHeaders(headers: Seq[(String, String)], next: Unit => StepResult[T])

    /** Sets a given cookie to the response. */
    case SetCookie(cookie: Cookie, next: Unit => StepResult[T])

    /** Invokes the given cleaner. */
    case InvokeCleaner(cleaner: Cleaner, next: Unit => StepResult[T])

    /** Adds the cleaner into the processing chaing. */
    case AddCleaner(cleaner: Cleaner, next: ResourceCleaner[Step] => StepResult[T])

    /** Adds the resource (ignoring cleaner). */
    case AddResource[R, T](cleaner: Cleaner, resource: R, next: R => StepResult[T]) extends Suspension[T]

    /** Adds the resource command. */
    case AddResourceCleaner[R, T](
          cleaner: Cleaner,
          resource: R,
          next: ((R, ResourceCleaner[Step])) => StepResult[T]
        ) extends Suspension[T]
  end Suspension


  /** Coroutine module with all the typeclasses, etc... */
  private val routine = new Coroutine[Suspension]

  /** Implementation of the monad for the Step. */
  given monadInstance: Monad[Step] = routine.monadInstance


  /** Resource cleaner - how to clean resource. */
  private abstract class Cleaner
      extends Module.CleanupNode
      with ResourceCleaner[Step]:

    final override def clean(): Step[Unit] =
      if cleaned then return Monad.pure(())
      routine.suspend(new routine.Suspender[Unit] {
        override def encode[V](cont: Unit => routine.RunResult[V]): Suspension[V] =
          Suspension.InvokeCleaner(Cleaner.this, cont)
      })

  end Cleaner


  /** Simple cleaner - how to clean some resource. */
  private final class SimpleCleaner(cleanFn: () => Unit) extends Cleaner:
    override def cleanImpl(): Unit = cleanFn()
  end SimpleCleaner


  /** Cleaner of the given resource (uses instance and function). */
  private final class InstanceCleaner[R](instance: R, fn: R => Unit) extends Cleaner:
    override def cleanImpl(): Unit = fn(instance)
  end InstanceCleaner



  /** Request processing instance. */
  given processingInstance: Processing[Step] with
    override def abort[T](resp: Response): Step[T] =
      routine.suspend(new routine.Suspender[T] {
        override def encode[V](continue: T => routine.RunResult[V]): Suspension[V] =
          Suspension.Abort(resp)
      })

    override def addHeaders(headers: Seq[(String, String)]): Step[Unit] =
      routine.suspend(new routine.Suspender[Unit] {
        override def encode[V](cont: Unit => routine.RunResult[V]): Suspension[V] =
          Suspension.AddHeaders(headers, cont)
      })


    override def setCookie(cookie: Cookie): Step[Unit] =
      routine.suspend(new routine.Suspender[Unit] {
        override def encode[V](cont: Unit => routine.RunResult[V]): Suspension[V] =
          Suspension.SetCookie(cookie, cont)
      })


    override def cleanup(cleaner: => Unit): Step[ResourceCleaner[Step]] =
      val c = new SimpleCleaner(() => cleaner)
      routine.suspend(new routine.Suspender[ResourceCleaner[Step]] {
        override def encode[V](cont: ResourceCleaner[Step] => routine.RunResult[V]): Suspension[V] =
          Suspension.AddCleaner(c, cont)
      })


    override def withResource[R](resource: R, cleanup: R => Unit): Step[R] =
      val c = new InstanceCleaner(resource, cleanup)
      routine.suspend(new routine.Suspender[R] {
        override def encode[V](cont: R => routine.RunResult[V]): Suspension[V] =
          Suspension.AddResource(c, resource, cont)
      })


    override def withCleanableResource[R](
          resource: R,
          cleanup: R => Unit,
        ): Step[(R, ResourceCleaner[Step])] =
      val c = new InstanceCleaner(resource, cleanup)
      routine.suspend(new routine.Suspender[(R, ResourceCleaner[Step])] {
        override def encode[V](cont: ((R, ResourceCleaner[Step])) => routine.RunResult[V]): Suspension[V] =
          Suspension.AddResourceCleaner(c, resource, cont)
      })


  end processingInstance


end Module


object Module:

  /** A node in the cleanup handler chain. */
  private[qos] abstract class CleanupNode:
    /** Next cleaner in the chain. */
    private[Module] var next: CleanupNode = null

    /** Private cleaner in the chain. */
    private[Module] var prev: CleanupNode = null

    /** If cleaning was already performed. */
    protected final var cleaned: Boolean = false

    /** Cleans resource associated with this module. */
    private[Module] def cleanResource(): Unit =
      if cleaned then return
      cleaned = true
      cleanImpl()
    end cleanResource


    /**
     * Removes the cleaner from the list of the cleaners registered
     * for the request.
     */
    private[Module] def unlink(owner: RequestContext[_]): Unit =
      if next != null then
        next.prev = prev

      if prev == null then
        owner.cleaner = next
      else
        prev.next = next
    end unlink


    /** Adds the node into the list of all cleanup handlers. */
    private[Module] def link(owner: RequestContext[_]): Unit =
      next = owner.cleaner
      if next != null then next.prev = this
      owner.cleaner = this
    end link


    /** Performs actual cleanup logic. */
    def cleanImpl(): Unit
  end CleanupNode


  /**
   * Creates a new module with Quality-of-service support.
   * @tparam Qos user-driven way to describe quality of service.
   */
  def apply[Qos: Ordering](
      ): Module[Qos] =
    new Module()
end Module
