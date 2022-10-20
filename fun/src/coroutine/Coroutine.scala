package io.github.maxkar
package fun.coroutine

import fun.typeclass.Monad


/**
 * General coroutine monad/module. It provides on-demand constant-stack
 * evaluation with client-controlled suspension point.
 *
 * The evaluation model is similar to the `fun.instances.Unnest` monad with
 * the additional ability to "suspend" execution an the given point,
 * and give the client ability to react to such suspension and then resume
 * the rest of the computation.
 *
 * Instances of this class just capture data and delegate implementation to the
 * static Coroutine (apart from monad instance which explicitly caches result).
 * Although it is possible to just use methods and APIs of the `Coroutine` object,
 * using an instance (capturing Sus type) may be more convenient.
 *
 * @tparam Sus type constructor of the "suspended" execution. This is
 *   client-controlled type that encodes semantics of the suspension (yield value
 *   for generators, "read token" for input/output coroutines, etc...). The type
 *   parameter to Sus encodes return type of the continuation (and the final monad
 *   execution). Coroutines that don't need return value may use trivial type
 *   constructor like `type MySuspension[T] = DiscriminatedUnionType`.
 */
final class Coroutine[Sus[_]]:
  /** The type representing routine being executed with coroutine support. */
  type Routine[T] = Coroutine.Routine[Sus, T]

  /** Result of running coroutine. */
  type RunResult[T] = Coroutine.RunResult[Sus, T]

  /** Routine suspender. */
  type Suspender[T] = Coroutine.Suspender[Sus, T]

  /**
   * Creates a monad instance that suspends execution of the routine and delegates
   * "coroutine call" to the encoder and then the specific handler. The method
   * is intended to be called by the specific coroutine implementation and not
   * the application code.
   *
   * This method treats coroutine as a normal value-returning method. Value of
   * that coroutine call may be used later thus the result of the `suspend` call
   * is regular value of the monadic type.
   *
   * @param encoder encoder for encoding suspension reason and continuation function
   *   into the format understood by the "coroutine invoker" (code that invoked
   *   the `run` method).
   * @return monad that returns the value of the "coroutine" invoked.
   */
  inline def suspend[T](encoder: Coroutine.Suspender[Sus, T]): Routine[T] =
    Coroutine.suspend(encoder)


  /** Monad implementation for the coroutine. */
  given monadInstance: Monad[Routine] = Coroutine.monadInstance[Sus]


  /** Runs the monad and returns either the final result or suspension description. */
  inline def run[T](routine: Routine[T]): RunResult[T] =
    Coroutine.run(routine)
end Coroutine


object Coroutine:
  /**
   * Result of the execution of the suspension.
   * @tparam Sus type constructor of the suspension information.
   * @tparam T of the execution.
   */
  enum RunResult[Sus[_], T]:
    /**
     * The execution was suspended.
     * @param reason reason for which the execution was suspended.
     */
    case Suspended(reason: Sus[T])
    /**
     * The "routine" completed with the given result.
     * @param result result of routine evaluation.
     */
    case Finished(result: T)
  end RunResult


  /** Encoding of the computation step(s). */
  abstract sealed class Routine[Sus[_], +V]

  /** Basic routine - something that has "simple" non-map value. */
  private abstract sealed class SimpleRoutine[Sus[_], +V] extends Routine[Sus, V]

  /** "Pure" value - used as input for next steps. */
  private case class Pure[Sus[_], +V](value: V) extends SimpleRoutine[Sus, V]

  /**
   * Suspension - an instruction for the client to perform coroutine-specific step.
   * The instruction contains suspender (aka factory) because specific continuation
   * function is not (yet) known. The actual continuation function will be created
   * during the routine evaluation and will depend on the next operation (if any)
   * applied to the returned value. I.e. the actual continuation will depend on
   * `flatMap`s applied to the value returned from the `Continuation.suspend` call.
   * @tparam Sus suspension encoding type constructor.
   * @tparam V type of the result returned from the coroutine.
   * @param suspender encoder used to encode the "suspension reason" to the client.
   */
  private case class Suspend[Sus[_], V](
        suspender: Suspender[Sus, V],
      ) extends SimpleRoutine[Sus, V]

  /**
   * Flat map routine.
   * @param base base computation. This is always a simple routine (i.e. value or suspension).
   *   Monad implementation of the coroutine rewrites the code according to monad rules to
   *   always have well-formed FlatMap nodes.
   * @param fn function to execute on the base value.
   */
  private case class FlatMap[Sus[_], V, R](
        base: SimpleRoutine[Sus, V],
        fn: V => Routine[Sus, R],
      ) extends Routine[Sus, R]


  /**
   * Customizable encoding of the suspension point.
   * The coroutine functionality has to pause the execution, indicate suspension
   * reason, and provide a way to resume the execution. This trait is responsible
   * for capturing and encoding buth suspension reason (this is part of suspender
   * implementation) and the way to resume execution (this is passed to the factory
   * method).
   *
   * @tparam Sus suspension type constructor - the way suspension "reason"
   *   is encoded for the client.
   * @tparam T type of the value "returned" from the corresponding "suspendable"
   *   operation. Value of this type will be available in the "routine" when
   *   execution is resumed.
   */
  trait Suspender[Sus[_], T]:
    /**
     * Encodes the suspension reason and "resume" function in the
     * way the coroutine driver could understand this.
     * @tparam V type of the "routine" that is being suspended.
     * @param continue function to resume execution of the "routine".
     *   The function takes result of the suspension ("everything is an
     *   expression" so suspension also has some value) and runs until the
     *   routine is complete or another suspension point occurs.
     */
    def encode[V](continue: T => RunResult[Sus, V]): Sus[V]
  end Suspender


  /**
   * Creates a monad instance that suspends execution of the routine and delegates
   * "coroutine call" to the encoder and then the specific handler. The method
   * is intended to be called by the specific coroutine implementation and not
   * the application code.
   *
   * This method treats coroutine as a normal value-returning method. Value of
   * that coroutine call may be used later thus the result of the `suspend` call
   * is regular value of the monadic type.
   *
   * @param encoder encoder for encoding suspension reason and continuation function
   *   into the format understood by the "coroutine invoker" (code that invoked
   *   the `run` method).
   * @return monad that returns the value of the "coroutine" invoked.
   */
  inline def suspend[Sus[_], T](encoder: Coroutine.Suspender[Sus, T]): Routine[Sus, T] =
    Suspend(encoder)


  /** Monad instance (factory) for the given Sus type. */
  given monadInstance[Sus[_]]: Monad[({type M[T] = Routine[Sus, T]})#M] with
    override def pure[T](v: T): Routine[Sus, T] =
      Coroutine.Pure(v)

    override def bind[S, R](v: Routine[Sus, S], fn: S => Routine[Sus, R]): Routine[Sus, R] =
      v match
        case a@Coroutine.Pure(_) => Coroutine.FlatMap(a, fn)
        case a@Coroutine.Suspend(_) => Coroutine.FlatMap(a, fn)
        case a@Coroutine.FlatMap(base, fn1) => Coroutine.FlatMap(base, x => bind(fn1(x), fn))
      end match
    end bind
  end monadInstance



  /** Runs the monad and returns either the final result or suspension description. */
  def run[Sus[_], T](routine: Routine[Sus, T]): RunResult[Sus, T] =
    var cur = routine
    while true do
      cur match
        case Pure(v) => return RunResult.Finished(v)
        case Suspend(sus) =>
          /* We have a monad that completely consists of couroutine call. It may be something
           * stupid (or end result of rewrites) but we have to handle this situation.
           * Technically, we suspend and when suspension is resolved just return result. I.e.
           * we use sus.encode(x => run(pure(x))) which after a couple optimization yields
           * the code below.
           */
          return RunResult.Suspended(sus.encode(RunResult.Finished.apply))
        case FlatMap(Pure(x), fn) => cur = fn(x)
        case FlatMap(Suspend(sus), fn) =>
          /* This one may also be written in a few different ways. For example, we may
           * encode `x => run(pure(x).flatMap(fn))` but this is just shorter.
           */
          return RunResult.Suspended(sus.encode(x => run(fn(x))))
      end match
    end while
    throw new Error("Uncheacheable code reached")
  end run
end Coroutine
