package io.github.maxkar
package fun.typeclass

/** Standard Applicative typeclass. */
trait Applicative[M[_]] extends Functor[M]:
  /** Creates a "pure" value from regular value. */
  def pure[T](v: T): M[T]


  /** Applicative application. Renamed to avoid name confusion. */
  def aapply[S, R](v: M[S], fn: M[S => R]): M[R]


  override def fmap[S, R](v: M[S], fn: S => R): M[R] =
    aapply(v, pure(fn))


  extension [S, R](fn: M[S => R])
    inline infix def ≻(v: M[S]): M[R] =
      Applicative.this.aapply(v, fn)

end Applicative



object Applicative:
  inline def pure[M[_], T](v: T)(using app: Applicative[M]): M[T] =
    app.pure(v)

end Applicative
