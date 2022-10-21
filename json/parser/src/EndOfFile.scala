package io.github.maxkar
package json.parser

import fun.typeclass.Monad

import text.input.LookAheadStream

/** End-of-file functionality for JSON. */
object EndOfFile:
  /**
   * End-of-file expectation errors.
   * @tparam M execution monad.
   * @tparam S type of the stream (context) used by the computation.
   */
  trait Errors[M[_], -S]:
    /** Encodes an error where end-of-file was expected but something else was present. */
    def endOfFileExpected(stream: S): M[Unit]
  end Errors


  /**
   * Validates that the stream is immediately at the end of file and raises
   * an error if it is not.
   */
  def expectImmediately[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S,
      )(implicit
        errs: Errors[M, S],
      ): M[Unit] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 then
        Monad.pure(())
      else
        errs.endOfFileExpected(stream)
    }
  end expectImmediately


  /**
   * Consumes the whitespaces and ensures the stream is at the end of file.
   */
  def expectNoValues[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S,
      )(implicit
        errs: Errors[M, S],
      ): M[Unit] =
    Whitespaces.skipAll(stream) flatMap { _ => expectImmediately(stream) }
  end expectNoValues
end EndOfFile
