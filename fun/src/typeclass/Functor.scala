package io.github.maxkar
package fun.typeclass

/** A general Functor typeclass. */
trait Functor[M[_]] {
  /** Functor's MAP function. */
  def fmap[S, R](v: M[S], fn: S => R): M[R]


  extension [S](x: M[S]) {
    inline infix def map[R](fn: S => R): M[R] = fmap(x, fn)

    inline infix def >->[R](fn: S => R): M[R] = fmap(x, fn)

    inline infix def >-|[R](res: => R): M[R] = fmap(x, _ => res)
  }


  extension [S, R](fn: S => R) {
    inline infix def <-<(v: M[S]): M[R] = fmap(v, fn)
  }
}
