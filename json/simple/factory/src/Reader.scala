package io.github.maxkar
package json.simple

import fun.typeclass.Monad

import text.input.LookAheadStream

import json.parser.Values
import json.parser.Values.AllErrors
import json.parser.EndOfFile

/**
 * Json value reading APIs (for simple JSON model).
 */
object Reader:
  /** Reads a simple value from the stream and stops after the value was read. */
  def readOneValue[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S,
      )(implicit
        errors: AllErrors[M, S],
      ): M[Json] =
    Values.readSimple(Builder, stream)
  end readOneValue


  /**
   * Reads value from the stream ensuring that no other data is contained in
   * the `stream`. In other words, it reads the **whole** stream as a single
   * JSON value.
   */
  def read[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S
      )(implicit
        errors: AllErrors[M, S],
        eofErrors: EndOfFile.Errors[M, S]
      ): M[Json] =
    for {
      res <- readOneValue(stream)
      _ <- EndOfFile.expectNoValues(stream)
    } yield res
  end read
end Reader
