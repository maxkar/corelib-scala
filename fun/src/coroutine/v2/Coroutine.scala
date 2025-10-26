package io.github.maxkar
package fun.coroutine.v2

import fun.typeclass.Monad

/**
 * A cooperative method execution where a "cooperative" part of
 * the execution is encoded as `CallTarget` instances.
 */
abstract sealed class Coroutine[CallTarget[_], +T] {
  /**
   * Executes the coroutine (on the current) thread until it completes
   * or some cooperative action should be performed.
   */
  final def run(): Flow[CallTarget, T] = Coroutine.runImpl(this)
}


object Coroutine {
  /** Simple ("pure") value. */
  private case class Pure[CallTarget[_], +V](value: V)
    extends Coroutine[CallTarget, V]

  /** Call of some external procedure not managed by the Coroutine runtime. */
  private case class Call[CallTarget[_], T](target: CallTarget[T])
    extends Coroutine[CallTarget, T]

  /** A function should be applied to a result of a coroutine. */
  private case class FlatMap[CallTarget[_], V, R](
        base: Coroutine[CallTarget, V],
        fn: V => Coroutine[CallTarget, R],
      ) extends Coroutine[CallTarget, R]


  /** Creates an "external call" coroutine with the provided target. */
  def call[CallTarget[_], V](target: CallTarget[V]): Coroutine[CallTarget, V] =
    new Call(target)


  /** Monad implementation for the routine type. */
  given monadInstance[CallTarget[_]]: Monad[[T] =>> Coroutine[CallTarget, T]] with {
    override def pure[T](v: T): Coroutine[CallTarget, T] =
      Pure(v)

    override def bind[S, R](
          v: Coroutine[CallTarget, S],
          fn: S => Coroutine[CallTarget, R]
        ): Coroutine[CallTarget, R] =
      FlatMap(v, fn)
  }


  /** Runs the routine until it completes or until it requests to perform an external operation. */
  private def runImpl[CallTarget[_], T](routine: Coroutine[CallTarget, T]): Flow[CallTarget, T] = {
    var cur = routine
    while true do {
      cur match {
        case Pure(value) =>
          return Flow.Done(value)
        case Call(token) =>
          return Flow.Call(token, Flow.Done.apply)
        case FlatMap(Pure(v), fn) =>  cur = fn(v)
        case FlatMap(Call(token), fn) =>
          return Flow.Call(token, fn(_).run())
        case FlatMap(FlatMap(base, fn1), fn) =>
          cur = FlatMap(base, x => FlatMap(fn1(x), fn))
      }
    }
    throw new Error("Uncheacheable code reached")
  }
}
