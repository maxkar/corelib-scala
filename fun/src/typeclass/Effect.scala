package io.github.maxkar
package fun.typeclass

/**
 * A definition of a monad that has some native effects (creating/modifying mutable
 * object, modifying file or database).
 */
trait Effect[M[_]] {
  /**
   * Creates a monad that perfroms an operation described by the `effect`.
   * Unlike Monad.pure, the effect may be evaluated lazily.
   */
  def apply[T](effect: => T): M[T]
}

object Effect {
  /**
   * Creates a monad that perfroms an operation described by the `effect`.
   * Unlike Monad.pure, the effect may be evaluated lazily.
   */
  inline def apply[M[_], T](effect: => T)(using eff: Effect[M]): M[T] =
    eff(effect)


  /**
   * Creates a monad that executes an effect and then also produces the original monad.
   */
  inline def make[M[_]: Monad, T](effect: => M[T])(using eff: Effect[M]): M[T] =
    Monad.flatten(eff(effect))
}
