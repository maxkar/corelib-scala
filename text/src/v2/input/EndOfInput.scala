package io.github.maxkar
package text.v2.input

import fun.typeclass.Monad

/** A trait that allows checking end of input. */
trait EndOfInput[M[_], -S] {
  def atEndOfInput(stream: S): M[Boolean]

  extension (base: S) {
    inline def atEnd(): M[Boolean] = atEndOfInput(base)
  }
}

/** Utilities for checking end of input. */
object EndOfInput {
  trait Errors[M[_], S] {
    /** Handles end of input condition. */
    def missingEndOfInput[T](stream: S): M[T]
  }
  type ErrorsIn[M[_]] = [T] =>> Errors[M, T]

  extension [M[_]: Monad, S](stream: S)(using eof: EndOfInput[M, S], errs: Errors[M, S]) {
    /** Ensures that the stream is at its end. */
    def ensureAtEnd(): M[Unit] =
      stream.atEnd() <||| {
        case true => Monad.pure(())
        case _ => errs.missingEndOfInput(stream)
      }
  }
}
