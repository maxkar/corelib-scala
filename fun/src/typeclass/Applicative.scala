package io.github.maxkar
package fun.typeclass

/** Standard Applicative typeclass. */
trait Applicative[M[_]] extends Functor[M] {
  /** Creates a "pure" value from regular value. */
  def pure[T](v: T): M[T]


  /** Applicative application. Renamed to avoid name confusion. */
  def aapply[S, R](v: M[S], fn: M[S => R]): M[R]


  override def fmap[S, R](v: M[S], fn: S => R): M[R] =
    aapply(v, pure(fn))


  extension [S, R](fn: M[S => R]) {
    inline infix def <=<(v: M[S]): M[R] = aapply(v, fn)
  }


  extension [S](v: M[S]) {
    inline infix def >=>[R](fn: M[S => R]): M[R] = aapply(v, fn)

    final infix def >=|[R](other: M[R]): M[R] =
      v >-> Applicative.second[S, R] <=< other
  }
}


object Applicative {
  inline def pure[M[_], T](v: T)(using app: Applicative[M]): M[T] =
    app.pure(v)

  private def second[F, S](f: F)(s: S): S = s
}
