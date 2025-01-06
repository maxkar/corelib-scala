package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad

import text.v2.input.Reader
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn


/**
 * Stateful and convenient reader of the array.
 *
 * The reader should be used in the loop similar to
 *
 * ```
 * var rd = ArrayReader(stream)
 * while rd.advanceToNext() do {
 *   readValue(stream)
 * }
 * ```
 *
 * @param stream stream used to read the array.
 * @param skipWhites function to skip whitespaces.
 */
final class ArrayReader[M[_]: Monad, S: LooksAheadIn[M]: ArrayReader.ErrorsIn[M]] private(
      stream: S,
      skipWhites: S => M[Unit]) {
  import ArrayReader.*

  /** State of the reading. */
  private var state = State.BeforeArray


  /** Checks if the array has next element. */
  def advanceToNext(): M[Boolean] =
    state match {
      case State.BeforeArray =>
        for {
          _ <- ArrayReader.readArrayStart(stream)
          _ <- skipWhites(stream)
          hasValue <- ArrayReader.hasFirstElement(stream)
        } yield {
          state = if hasValue then State.InsideArray else State.Eof
          hasValue
        }
      case State.InsideArray =>
        skipWhites(stream) <+> ArrayReader.readArraySeparatorOrEnd(stream) <||| { hasNext =>
          if hasNext then
            skipWhites(stream) <+> Monad.pure(true)
          else {
            state = State.Eof
            Monad.pure(false)
          }
        }
      case State.Eof => Monad.pure(false)
    }


  /** Reads the array as a sequence. */
  def readSequence[E](readElement: S => M[E]): M[Seq[E]] = {
    val agg = new scala.collection.mutable.ArrayBuffer[E]()
    def step(): M[Seq[E]] = {
      advanceToNext() <||| {
        case true =>
          readElement(stream) <||| { elt =>
            agg += elt
            step()
          }
        case false => Monad.pure(agg.toSeq)
      }
    }
    step()
  }
}


object ArrayReader {

  /** Error encoding for array formats. */
  trait Errors[M[_], -S] {
    /**
     * Invoked when array start was expected but something different occured in the stream.
     * Stream position is before the character that was expected to be array start.
     */
    def invalidArrayStart[T](stream: S): M[T]

    /**
     * Invoked when array end or value separator was expected
     * but something different occured in the stream.
     * Stream position is before the character that should be array end or
     * value separator.
     */
    def invalidArrayEnd[T](stream: S): M[T]
  }
  type ErrorsIn[M[_]] = [S] =>> Errors[M, S]


  object Errors {
    def raise[M[_], S](raiseFn: [T] => (S, String) => M[T]): Errors[M, S] =
      new Errors[M, S] {
        override def invalidArrayStart[T](stream: S): M[T] =
          raiseFn(stream, "Invalid array start")
        override def invalidArrayEnd[T](stream: S): M[T] =
          raiseFn(stream, "Invalid array separator or end")
      }
  }


  private enum State {
    case BeforeArray
    case InsideArray
    case Eof
  }


  /** Creates a new array reader with the specificed function to skip whitespaces. */
  def apply[M[_]: Monad, S: LooksAheadIn[M]: ErrorsIn[M]](
        stream: S,
        skipWhitespaces: S => M[Unit]
      ): ArrayReader[M, S] =
    new ArrayReader(stream, skipWhitespaces)


  /** Creates a new array reader with the default function to skip whitespaces. */
  def apply[M[_]: Monad, S: LooksAheadIn[M]: ErrorsIn[M]](stream: S): ArrayReader[M, S] =
    ArrayReader(stream, s => WhitespaceReader(s).skipAll())


  /** Checks if character is an array start character. */
  def isArrayStart(char: Int): Boolean =
    char == '['


  /** Checks if character is indicator of array end character. */
  def isArrayEnd(char: Int): Boolean =
    char == ']'


  /** Checks if character is array element separator. */
  def isArraySeparator(char: Int): Boolean =
    char == ','


  /** Reads the array opening character. */
  def readArrayStart[M[_]: Monad, S: LooksAheadIn[M]](
        stream: S
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    stream.peek(0) <||| {
      case '[' => stream.skip(1)
      case _ => errs.invalidArrayStart(stream)
    }


  /**
   * Checks if there is a first element or reads end of array. Consumes
   * the end of the array (if the array is empty).
   */
  def hasFirstElement[M[_]: Monad, S: LooksAheadIn[M]](stream: S): M[Boolean] =
    stream.peek(0) <||| {
      case ']' => stream.skip(1) <| { _ => false }
      case _ => Monad.pure(true)
    }


  /**
   * Reads the array elemnt separator or array end.
   * Returns `true` if the array element separator was read.
   * Returns `false` if the array end was read.
   * Raises an error in all other cases.
   */
  def readArraySeparatorOrEnd[M[_]: Monad, S: LooksAheadIn[M]](
        stream: S
      )(using
        errs: Errors[M, S]
      ): M[Boolean] =
    stream.peek(0) <||| {
      case ',' => stream.skip(1) <| { _ => true }
      case ']' => stream.skip(1) <| { _ => false }
      case _ => errs.invalidArrayEnd(stream)
    }
}
