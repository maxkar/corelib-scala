package io.github.maxkar
package fun.coroutine

import fun.typeclass.Monad


/**
 * General coroutine monad/module. It provides on-demand constant-stack
 * evaluation with client-controlled "external" evaluation.
 *
 * Instances of this class just capture data and delegate implementation to the
 * static Coroutine (apart from monad instance which explicitly caches result).
 * Although it is possible to just use methods and APIs of the `Coroutine` object,
 * using an instance (capturing Op type) may be more convenient.
 *
 * @tparam Op (super-)type encoding an "external" process.
 */
final class Coroutine[Op[_]]:
  /** The type representing routine being executed with coroutine support. */
  type Routine[T] = Coroutine.Routine[Op, T]

  /** Result of running the routine. */
  type RunResult[T] = Coroutine.RunResult[Op, T]

  /**
   * Creates a new suspension represented by the given token.
   * @param token token describing "what" should be done externally to the routine.
   */
  inline def suspend[T](token: Op[T]): Routine[T] =
    Coroutine.suspend(token)

  /** Monad implementation for the routine. */
  given monadInstance: Monad[Routine] = Coroutine.monadInstance[Op]


  /** Runs the monad and returns either the final result or suspension description. */
  inline def run[T](routine: Routine[T]): RunResult[T] =
    Coroutine.run(routine)
end Coroutine


/**
 * A basic coroutine implementation.
 *
 * The coroutine use some form of "typed" tokens to encode coroutines and then builds
 * execution around the concept. The Open[T] encodes some "external process" that will
 * provide value of the type T.
 */
object Coroutine:
  /**
   * Result of running some routine with a token-based suspension.
   * @tparam Op (super-) type of the token representing an external operation. The
   *   actual type's argument is the value that is supposed to be returned from the
   *   corresponding operation.
   */
  enum RunResult[Op[_], T]:
    /**
     * Routine evaluation finished with the given outcome.
     * @param result result of the routine evalutaion.
     */
    case Finished(result: T)


    /**
     * The execution was suspended (with the given token) for an external operation.
     * @tparam Op (super-)type of the token representing an external operation.
     * @tparam Cor specific type that is "returned" by the external operation.
     * @tparam T type of the whole routine (not the external operation).
     * @param token token encoding the requested operation.
     * @param continue function to resume routine evalutaion after value is available
     *   from some external source.
     */
    case Suspended[Op[_], Cor, T](
          token: Op[Cor],
          continue: Cor => Routine[Op, T],
        ) extends RunResult[Op, T]
  end RunResult


  /**
   * Routine - something that could be evaluated (with some processes
   * being executed outside).
   * @tparam Op (super-)type used to encode external procesess.
   * @tparam V type that is produced by the routine.
   */
  enum Routine[Op[_], +V]:
    /** Just a value. */
    case Pure(value: V)

    /** Standard monadic flatmap operation. */
    case FlatMap[Op[_], V, R](
          base: Routine[Op, V],
          fn: V => Routine[Op, R]
        ) extends Routine[Op, R]

    /** Suspension with the given token. */
    case Suspend[Op[_], V](token: Op[V]) extends Routine[Op, V]
  end Routine



  /**
   * Supsends the execution with the given token.
   * @tparam Op general (super-)type of all the suspension tokens.
   * @tparam T type returned by the process encoded by the token.
   * @param token token describing the "external" process yielding value of type T.
   */
  inline def suspend[Op[_], T](token: Op[T]): Routine[Op, T] =
    Routine.Suspend(token)


  /** Monad implementation for the routine type. */
  given monadInstance[Op[_]]: Monad[({type M[T] = Routine[Op, T]})#M] with
    override def pure[T](v: T): Routine[Op, T] =
      Routine.Pure(v)

    override def bind[S, R](v: Routine[Op, S], fn: S => Routine[Op, R]): Routine[Op, R] =
      Routine.FlatMap(v, fn)
  end monadInstance


  /** Runs the routine until it completes or until it requests to perform an external operation. */
  def run[Op[_], T](routine: Routine[Op, T]): RunResult[Op, T] =
    var cur = routine
    while true do
      cur match
        case Routine.Pure(value) => return RunResult.Finished(value)
        case Routine.Suspend(token) =>
          /* The whole routine was just a call to the coroutine.
           * We still have to encode this into the standard "continuation-based" API.
           * We do this by re-writing "sus" to "sus.flatMap(pure)" which now could
           * be expressed via the "RunResult.Suspended"
           */
          return RunResult.Suspended(token, Routine.Pure.apply)
        case Routine.FlatMap(Routine.Pure(v), fn) =>  cur = fn(v)
        case Routine.FlatMap(Routine.Suspend(token), fn) =>
          return RunResult.Suspended(token, fn)
        case Routine.FlatMap(Routine.FlatMap(base, fn1), fn) =>
          cur = Routine.FlatMap(base, x => Routine.FlatMap(fn1(x), fn))
      end match
    end while
    throw new Error("Uncheacheable code reached")
  end run

end Coroutine
