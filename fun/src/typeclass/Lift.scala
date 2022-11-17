package io.github.maxkar
package fun.typeclass


/**
 * The "lift" functionality - converter from one type constructor
 * to another. Often used to integrate various monads with each other
 * where "lower" (less sophisticated) monad is **lifted** to become
 * "higher" (more sophisticated) monad. For example, simple `Future`
 * could be lifted into a combination of State, Future and Non-local
 * control flow monad.
 *
 * @tparam S source type contsructor.
 * @tparam D destination type constructor.
 */
trait Lift[S[_], D[_]]:
  /**
   * Converts value from source type into the destination type.
   */
  def apply[T](v: S[T]): D[T]
end Lift


object Lift:
  /**
   * A simple type alias capturing the first (source) type variable.
   * Intended to be used as a type bound like
   * ```[M[_]: Lift.From[Future]]```
   */
  type From[S[_]] = [D[_]] =>> Lift[S, D]


  /** "Stand-alone" synonym for the Lift function. */
  @inline
  def apply[S[_], D[_], T](v: S[T])(using lift: Lift[S, D]): D[T] =
    lift(v)
end Lift
