package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad

import text.v2.input.Reader
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn

/**
 * Stateful and convenient reader of the object.
 *
 * The reader should be used in the loop similar to
 *
 * ```
 * var rd = ObjectReader(stream)
 * while rd.advanceToNext() do {
 *   val key = readKey(stream)
 *   rd.skipKeyValueSeparator()
 *   val value = readValue(stream)
 *   // result.add(key, value)
 * }
 * ```
 *
 * @param stream stream used to read the array.
 * @param skipWhites function to skip whitespaces.
 */
final class ObjectReader[M[_]: Monad, S: LooksAheadIn[M]: ObjectReader.ErrorsIn[M]] private(
      stream: S,
      skipWhites: S => M[Unit]) {
  import ObjectReader.*

  /** State of the reading. */
  private var state = State.BeforeObject


  /** Checks if the array has next element. */
  def advanceToNext(): M[Boolean] =
    state match {
      case State.BeforeObject =>
        for {
          _ <- ObjectReader.readObjectStart(stream)
          _ <- skipWhites(stream)
          hasValue <- ObjectReader.hasFirstPair(stream)
        } yield {
          state = if hasValue then State.InsideObject else State.Eof
          hasValue
        }
      case State.InsideObject =>
        skipWhites(stream) <+> ObjectReader.readEntrySeparatorOrEnd(stream) <||| { hasNext =>
          if hasNext then
            skipWhites(stream) <+> Monad.pure(true)
          else {
            state = State.Eof
            Monad.pure(false)
          }
        }
      case State.Eof => Monad.pure(false)
    }


  /** Skips key-value separator and its surrounding whitespaces. */
  def readKeyValueSeparator(): M[Unit] =
    skipWhites(stream) <+> ObjectReader.readKeyValueSeparator(stream) <+> skipWhites(stream)


  /** Reads the object as a map. */
  def readMap[K, V](readKey: S => M[K], readValue: S => M[V]): M[Map[K, V]] = {
    val agg = new scala.collection.mutable.HashMap[K, V]()
    def step(): M[Map[K, V]] = {
      advanceToNext() <||| {
        case true =>
          for {
            key <- readKey(stream)
            _ <- readKeyValueSeparator()
            value <- readValue(stream)
            _ = agg.put(key, value)
            res <- step()
          } yield
            res
        case false => Monad.pure(agg.toMap)
      }
    }
    step()
  }
}

object ObjectReader {

  /** Error encoding for array formats. */
  trait Errors[M[_], -S] {
    /*
     * Invoked when object start was expected but something different occured in the stream.
     * Stream position is before the character that was expected to be object start.
     */
    def invalidObjectStart[T](stream: S): M[T]

    /**
     * Invoked when object end or object entry separator was expected
     * but something different occured in the stream.
     * Stream position is before the character that should be object end or object entity
     * separator.
     */
    def invalidObjectEnd[T](stream: S): M[T]

    /**
     * Invoked when key-value separator was expected but something different occured on the stream.
     * Stream position is before the character that should be key-value separator.
     */
    def invalidKeyValueSeparator[T](stream: S): M[T]
  }
  type ErrorsIn[M[_]] = [S] =>> Errors[M, S]


  object Errors {
    def raise[M[_], S](raiseFn: [T] => (S, String) => M[T]): Errors[M, S] =
      new Errors[M, S] {
        override def invalidObjectStart[T](stream: S): M[T] =
          raiseFn(stream, "Invalid object start")
        override def invalidObjectEnd[T](stream: S): M[T] =
          raiseFn(stream, "Invalid entry separator or object end")
        override def invalidKeyValueSeparator[T](stream: S): M[T] =
          raiseFn(stream, "Invalid key-value separator")
      }
  }


  private enum State {
    case BeforeObject
    case InsideObject
    case Eof
  }

  /** Creates a new array reader with the specificed function to skip whitespaces. */
  def apply[M[_]: Monad, S: LooksAheadIn[M]: ErrorsIn[M]](
        stream: S,
        skipWhitespaces: S => M[Unit]
      ): ObjectReader[M, S] =
    new ObjectReader(stream, skipWhitespaces)


  /** Creates a new array reader with the default function to skip whitespaces. */
  def apply[M[_]: Monad, S: LooksAheadIn[M]: ErrorsIn[M]](stream: S): ObjectReader[M, S] =
    ObjectReader(stream, s => WhitespaceReader(s).skipAll())


  /** Checks if the haracter is valid object start character. */
  def isObjectStart(char: Char): Boolean =
    char == '{'


  /** Checks if the haracter is valid object end character. */
  def isObjectEnd(char: Char): Boolean =
    char == '}'


  /** Checks if the character is valid separator between two key-value pairs. */
  def isEntrySeparator(char: Char): Boolean =
    char == ','


  /** Checks if the character is valid key-value separator. */
  def isKeyValueSeparator(char: Char): Boolean =
    char == ':'


  /** Reads the object opening character. */
  def readObjectStart[M[_]: Monad, S: LooksAheadIn[M]](
        stream: S
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    stream.peek(0) <||| {
      case '{' => stream.skip(1)
      case _ => errs.invalidObjectStart(stream)
    }


  /**
   * Checks if there is a first element or reads end of the object. Consumes
   * the end of the object (if the object is empty).
   */
  def hasFirstPair[M[_]: Monad, S: LooksAheadIn[M]](stream: S): M[Boolean] =
    stream.peek(0) <||| {
      case '}' => stream.skip(1) <| { _ => false }
      case _ => Monad.pure(true)
    }


  /** Reads key-value separator. */
  def readKeyValueSeparator[M[_]: Monad, S: LooksAheadIn[M]](
        stream: S
      )(using
        errs: Errors[M, S]
      ): M[Unit] =
    stream.peek(0) <||| {
      case ':' => stream.skip(1)
      case _ => errs.invalidKeyValueSeparator(stream)
    }


  /**
   * Reads the key pair elemnt separator or object end.
   * Returns `true` if the object entry separator was read.
   * Returns `false` if the object end was read.
   * Raises an error in all other cases.
   */
  def readEntrySeparatorOrEnd[M[_]: Monad, S: LooksAheadIn[M]](
        stream: S
      )(using
        errs: Errors[M, S]
      ): M[Boolean] =
    stream.peek(0) <||| {
      case ',' => stream.skip(1) <| { _ => true }
      case '}' => stream.skip(1) <| { _ => false }
      case _ => errs.invalidObjectEnd(stream)
    }
}
