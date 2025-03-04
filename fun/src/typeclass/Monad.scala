package io.github.maxkar
package fun.typeclass

import scala.annotation.targetName

/** Standard Monad typeclass. */
trait Monad[M[_]] extends Applicative[M] {
  /** Monadic bind operation */
  def bind[S, R](v: M[S], fn: S => M[R]): M[R]


  override def aapply[S, R](v: M[S], fn: M[S => R]): M[R] =
    bind(fn, fnv =>
      bind(v, vv =>
        pure(fnv(vv))
      )
    )


  def flatten[T](v: M[M[T]]): M[T] =
    bind(v, identity)


  extension [S](v: M[S]) {
    inline infix def flatMap[R](fn: S => M[R]) =
      Monad.this.bind(v, fn)

    def withFilter(fn: S => Boolean): M[S] =
      Monad.this.fmap(v,
        (vv: S) => if fn(vv) then vv else throw new Exception("No match in monad for comprehension")
      )

    inline infix def <|||[R](fn: S => M[R]) =
      Monad.this.bind(v, fn)

    /**
     * Evaluates "this" monad then runs continuation.
     * Continuation is passed by name as some monads may be lazy
     * (has to be started explicitly). We need to re-do calculation
     * if the lazy monad is "re-run" multiple times.
     */
    inline infix def <+>[R](continuation: => M[R]) =
      Monad.this.bind(v, _ => continuation)
  }


  extension [S, R](fn: S => M[R]) {
    inline infix def |||>(v: M[S]): M[R] =
      Monad.this.bind(v, fn)
  }


  extension [S, R](fn: M[S => M[R]]) {
    inline infix def |||>(v: M[S]): M[R] =
      Monad.this.flatten(Monad.this.aapply(v, fn))
  }
}


object Monad {
  inline def pure[M[_], T](v: T)(using app: Applicative[M]): M[T] =
    app.pure(v)

  inline def flatten[M[_], T](v: M[M[T]])(using md: Monad[M]): M[T] =
    md.flatten(v)
}
