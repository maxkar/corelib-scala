package io.github.maxkar
package fun.coroutine

import fun.typeclass.Monad


/**
 * General coroutine monad/module. It provides on-demand constant-stack
 * evaluation with client-controlled "external" evaluation.
 *
 * Instances of this class just capture data and delegate implementation to the
 * static Tokentine (apart from monad instance which explicitly caches result).
 * Although it is possible to just use methods and APIs of the `Tokentine` object,
 * using an instance (capturing Tok type) may be more convenient.
 *
 * @tparam Tok (super-)type encoding an "external" process.
 */
final class Tokentine[Tok[_]]:
  /** The type representing routine being executed with coroutine support. */
  type Routine[T] = Tokentine.Routine[Tok, T]

  /** Result of running the routine. */
  type RunResult[T] = Tokentine.RunResult[Tok, T]

  /**
   * Creates a new suspension represented by the given token.
   * @param token token describing "what" should be done externally to the routine.
   */
  inline def suspend[T](token: Tok[T]): Routine[T] =
    Tokentine.suspend(token)

  /** Monad implementation for the routine. */
  given monadInstance: Monad[Routine] = Tokentine.monadInstance[Tok]


  /** Runs the monad and returns either the final result or suspension description. */
  inline def run[T](routine: Routine[T]): RunResult[T] =
    Tokentine.run(routine)
end Tokentine


/**
 * Another approach to coroutine definition that may be easier to implement and use.
 *
 * The "tokentine" use some form of "typed" tokens to encode coroutines and then builds
 * execution around the concept. The Token[T] encodes some "external process" that will
 * provide value of the type T. This is very different from the Suspension used by coroutine
 * where Suspension[T] encodes **some** external process (with the type being defined by
 * whatever it was chosen by the specific client) and the approprate callback that will
 * yield another coroutine resulting in the type T. In simpler words, token's type T is
 * the type of **one** step and suspension's type is the type of the **remaining** computation.
 *
 * This change should greatly simplify implementation of suspensions at the potential cost of
 * (slightly) complicating the processing loop. Given that there are usually more suspensions
 * (at least one and very often more than one) than computation loops (usually only one), this
 * seems to be a good tradeoff.
 */
object Tokentine:
  /**
   * Result of running some routine with a token-based suspension.
   * @tparam Tok (super-) type of the token representing an external operation. The
   *   actual type's argument is the value that is supposed to be returned from the
   *   corresponding operation.
   */
  enum RunResult[Tok[_], T]:
    /**
     * Routine evaluation finished with the given outcome.
     * @param result result of the routine evalutaion.
     */
    case Finished(result: T)


    /**
     * The execution was suspended (with the given token) for an external operation.
     * @tparam Tok (super-)type of the token representing an external operation.
     * @tparam Cor specific type that is "returned" by the external operation.
     * @tparam T type of the whole routine (not the external operation).
     * @param token token encoding the requested operation.
     * @param continue function to resume routine evalutaion after value is available
     *   from some external source.
     */
    case Suspended[Tok[_], Cor, T](
          token: Tok[Cor],
          continue: Cor => Routine[Tok, T],
        ) extends RunResult[Tok, T]
  end RunResult


  /**
   * Routine - something that could be evaluated (with some processes
   * being executed outside).
   * @tparam Tok (super-)type used to encode external procesess.
   * @tparam V type that is produced by the routine.
   */
  enum Routine[Tok[_], +V]:
    /** Just a value. */
    case Pure(value: V)

    /** Standard monadic flatmap operation. */
    case FlatMap[Tok[_], V, R](
          base: Routine[Tok, V],
          fn: V => Routine[Tok, R]
        ) extends Routine[Tok, R]

    /** Suspension with the given token. */
    case Suspend[Tok[_], V](token: Tok[V]) extends Routine[Tok, V]
  end Routine



  /**
   * Supsends the execution with the given token.
   * @tparam Tok general (super-)type of all the suspension tokens.
   * @tparam T type returned by the process encoded by the token.
   * @param token token describing the "external" process yielding value of type T.
   */
  inline def suspend[Tok[_], T](token: Tok[T]): Routine[Tok, T] =
    Routine.Suspend(token)


  /** Monad implementation for the routine type. */
  given monadInstance[Tok[_]]: Monad[({type M[T] = Routine[Tok, T]})#M] with
    override def pure[T](v: T): Routine[Tok, T] =
      Routine.Pure(v)

    override def bind[S, R](v: Routine[Tok, S], fn: S => Routine[Tok, R]): Routine[Tok, R] =
      Routine.FlatMap(v, fn)
  end monadInstance


  /** Runs the routine until it completes or until it requests to perform an external operation. */
  def run[Tok[_], T](routine: Routine[Tok, T]): RunResult[Tok, T] =
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

end Tokentine
