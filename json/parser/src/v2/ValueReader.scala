package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn


/** Reader for all value types. */
object ValueReader {

  /**
   * Reader for the specifically typed JSON values.
   * @tparam S type of the input stream.
   * @tparam T type of the value.
   */
  trait ValueReader[S, T] {
    /** Reads the `true` literal. */
    def readTrue(stream: S): T
    /** Reads the `false` literal. */
    def readFalse(stream: S): T
    /** Reads the `null` literal. */
    def readNull(stream: S): T
    /** Reads a number. */
    def readNumber(stream: S): T
    /** Reads a string. */
    def readString(stream: S): T
    /** Reads an array. */
    def readArray(stream: S): T
    /** Reads an object. */
    def readObject(stream: S): T
  }

  trait Errors[M[_], S] {
    def invalidValue[T](stream: S): M[T]
  }
  type ErrorsIn[M[_]] = [T] =>> Errors[M, T]


  /** Reads value from the stream using the specified dispatch function. */
  def readValue[M[_]: Monad, S: LooksAheadIn[M], T](
        stream: S,
        dispatch: ValueReader[S, M[T]]
      )(using
        errs: Errors[M, S]
      ): M[T] =
    stream.peek(1) <||| {
      case 't' => dispatch.readTrue(stream)
      case 'f' => dispatch.readFalse(stream)
      case 'n' => dispatch.readNull(stream)
      case '"' => dispatch.readString(stream)
      case '{' => dispatch.readObject(stream)
      case '[' => dispatch.readArray(stream)
      case '-' => dispatch.readNumber(stream)
      case x if '0' <= x && x <= '9' => dispatch.readNumber(stream)
      case _ => errs.invalidValue(stream)
    }
}
