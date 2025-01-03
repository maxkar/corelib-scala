package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad
import text.v2.input.LookAhead
import text.v2.input.EndOfInput
import text.v2.input.LooksAheadIn

final class SimpleReader[M[_]: Monad, S: LooksAheadIn[M], V](
      builder: SimpleReader.Builder[V],
      errs: SimpleReader.Errors[M, S],
    ) extends ValueReader.ValueReader[S, M[V]] {
  import errs.given
  import text.v2.input.LookAhead.given

  private val literalReader = LiteralReader.all()

  def skipWhitespaces(stream: S): M[Unit] =
    WhitespaceReader(stream).skipAll()

  override def readTrue(stream: S): M[V] =
    literalReader.trueLiteral(stream) <| { _ => builder.fromBoolean(true) }

  override def readFalse(stream: S): M[V] =
    literalReader.falseLiteral(stream) <| { _ => builder.fromBoolean(false) }

  override def readNull(stream: S): M[V] =
    literalReader.nullLiteral(stream) <| { _ => builder.fromNull() }

  override def readString(stream: S): M[V] =
    new StringReader(stream).readString() <| builder.fromString

  override def readNumber(stream: S): M[V] =
    new NumberReader(stream).readString() <| builder.fromNumber

  override def readArray(stream: S): M[V] =
    ArrayReader(stream, skipWhitespaces).readSequence(readValue) <| builder.fromArray

  override def readObject(stream: S): M[V] =
    ObjectReader(stream, skipWhitespaces).readMap(readKey, readValue) <| builder.fromObject

  /** Reads a single value from the stream. */
  def readValue(stream: S): M[V] =
    skipWhitespaces(stream) <+> ValueReader.readValue(stream, this)


  /** Reads the value and ensures there is no other values in the stream. */
  def readFully(stream: S): M[V] =
    for {
      res <- readValue(stream)
      _ <- skipWhitespaces(stream)
      _ <- stream.ensureAtEnd()
    } yield res


  /** Reads object key. */
  private def readKey(stream: S): M[String] =
    new StringReader(stream).readString()
}

object SimpleReader {
  /**
   * Very simple builder of the JSON model.
   * @tparam T type of the JSON model element.
   */
  trait Builder[T] {
    /** Creates a new element from the boolean value. */
    def fromBoolean(v: Boolean): T
    /** Encodes explicit `null` value. */
    def fromNull(): T
    /**
     * Encodes numeric value.
     * @param repr JSON number representation that occured in the input.
     */
    def fromNumber(repr: String): T
    /** Encodes string value. */
    def fromString(value: String): T
    /** Encodes array value. */
    def fromArray(items: Seq[T]): T
    /** Encodes object value. */
    def fromObject(items: Map[String, T]): T
  }


  /**
   * All errors combined.
   * @tparam M execution monad.
   * @tparam S type of the stream (context) used by the computation.
   */
  trait Errors[M[_], S] {
    /** Value errors handlers. */
    given valueErrors: ValueReader.Errors[M, S]
    /** Literal error handlers. */
    given literalErrors: LiteralReader.Errors[M, S]
    /** Number error handlers. */
    given numberErrors: NumberReader.Errors[M, S]
    /** String error handlers. */
    given stringErrors: StringReader.Errors[M, S]
    /** Array error handlers. */
    given arrayErrors: ArrayReader.Errors[M, S]
    /** Object error handlers. */
    given objectErrors: ObjectReader.Errors[M, S]
    /** End-of-input checks. */
    given eofErrors: EndOfInput.Errors[M, S]
  }


  /** Creates a simple error handler where all errors are diverted to just text. */
  def simpleErrors[M[_], S](raise: [T] => (S, String) => M[T]): Errors[M, S] =
    new Errors[M, S] {
      override given valueErrors: ValueReader.Errors[M, S] with {
        override def invalidValue[T](stream: S): M[T] =
          raise(stream, "Invalid JSON Value")
      }

      override given literalErrors: LiteralReader.Errors[M, S] with {
        override def badLiteral[T](stream: S, expected: String, mismatchOffset: Int, actualChar: Int): M[T] =
          raise(stream, s"Invalid ${expected} literal")
      }

      override given numberErrors: NumberReader.Errors[M, S] with {
        override def leadingIntegerZero[T](stream: S): M[T] =
          raise(stream, "Leading integer 0")
        override def missingDecimalDigits[T](stream: S): M[T] =
          raise(stream, "Missing decimal digits")
        override def missingExponentDigits[T](stream: S): M[T] =
          raise(stream, "Missing exponent digits")
        override def missingIntegerDigits[T](stream: S): M[T] =
          raise(stream, "Missing integer digits")
      }

      override given stringErrors: StringReader.Errors[M, S] with {
        override def illegalStringStart[T](stream: S): M[T] =
          raise(stream, "Illegal string start")
        override def invalidCharacter[T](stream: S): M[T] =
          raise(stream, "Invalid character")
        override def invalidEscapeCharacter[T](stream: S): M[T] =
          raise(stream, "Invalid escape character")
        override def invalidUnicodeEscape[T](stream: S): M[T] =
          raise(stream, "Invalid unicode escape")
        override def unterminatedString[T](stream: S): M[T] =
          raise(stream, "Unterminated string")
      }

      override given arrayErrors: ArrayReader.Errors[M, S] with {
        override def invalidArrayStart[T](stream: S): M[T] =
          raise(stream, "Invalid array start")
        override def invalidArrayEnd[T](stream: S): M[T] =
          raise(stream, "Invalid array end")
      }

      override given objectErrors: ObjectReader.Errors[M, S] with {
        override def invalidObjectStart[T](stream: S): M[T] =
          raise(stream, "Invalid object start")
        override def invalidObjectEnd[T](stream: S): M[T] =
          raise(stream, "Invalid object end")
        override def invalidKeyValueSeparator[T](stream: S): M[T] =
          raise(stream, "Invalid key/value separator")
      }

      override given eofErrors: EndOfInput.Errors[M, S] with {
        override def missingEndOfInput[T](stream: S): M[T] =
          raise(stream, "Trailing data, expected EOF")
      }
    }
}
