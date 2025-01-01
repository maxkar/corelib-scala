package io.github.maxkar
package text.v2.input

import fun.typeclass.Monad

/** A trait that allows checking end of input. */
trait EndOfInput[M[_], S] {
  def atEndOfInput(stream: S): M[Boolean]

  extension (base: S) {
    inline def atEnd(): M[Boolean] = atEndOfInput(base)

    /** Ensures that the stream is at its end. */
    def ensureAtEnd()(using md: Monad[M], errs: EndOfInput.Errors[M, S]): M[Unit] =
      atEnd() <||| {
        case true => Monad.pure(())
        case _ => errs.missingEndOfInput(base)
      }
  }
}

/** Utilities for checking end of input. */
object EndOfInput {
  trait Errors[M[_], S] {
    /** Handles end of input condition. */
    def missingEndOfInput[T](stream: S): M[T]
  }
  type ErrorsIn[M[_]] = [T] =>> Errors[M, T]
}
