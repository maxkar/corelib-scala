package io.github.maxkar
package fun

/** A general Functor typeclass. */
trait Functor[M[_]]:
  /** Functor's MAP function. */
  def fmap[S, R](v: M[S], fn: S => R): M[R]

  extension [S](x: M[S])
    inline def map[R](fn: S => R): M[R] = 
      Functor.this.fmap(x, fn)

    inline infix def ≺[R](fn: S => R): M[R]  =
      Functor.this.fmap(x, fn)


  extension [S, R](fn: S => R)
    inline infix def ≻(v: M[S]): M[R] =
      Functor.this.fmap(v, fn)

end Functor
