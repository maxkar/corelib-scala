package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn

final class SimpleReader[M[_]: Monad, S: LooksAheadIn[M], V](
      builder: SimpleReader.Builder[V],
      errs: SimpleReader.Errors[M, S],
    ) extends ValueReader.ValueReader[S, M[V]] {
  import errs.given

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
      _ <- ensureAtEnd(stream)
    } yield res


  /** Reads object key. */
  private def readKey(stream: S): M[String] =
    new StringReader(stream).readString()


  /** Checks if the stream is at the end. */
  private def ensureAtEnd(stream: S): M[Unit] =
    stream.atEnd() <| {
      case true => Monad.pure(())
      case false => errs.trailingData(stream)
    }
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
    /** Handles a case where there is trailing data after value. */
    def trailingData[T](stream: S): M[T]
  }


  object Errors {
    /** Creates a simple error handler where all errors are diverted to just text. */
    def raise[M[_], S](raiseFn: [T] => (S, String) => M[T]): Errors[M, S] =
      new Errors[M, S] {
        override given valueErrors: ValueReader.Errors[M, S] =
          ValueReader.Errors.raise(raiseFn)
        override given literalErrors: LiteralReader.Errors[M, S] =
          LiteralReader.Errors.raise(raiseFn)
        override given numberErrors: NumberReader.Errors[M, S] =
          NumberReader.Errors.raise(raiseFn)
        override given stringErrors: StringReader.Errors[M, S] =
          StringReader.Errors.raise(raiseFn)
        override given arrayErrors: ArrayReader.Errors[M, S] =
          ArrayReader.Errors.raise(raiseFn)
        override given objectErrors: ObjectReader.Errors[M, S] =
          ObjectReader.Errors.raise(raiseFn)
        override def trailingData[T](stream: S): M[T] =
          raiseFn(stream, "Trailing data not allowed after value")
      }
  }


  /** Creates a new parser with the given object builder. */
  def apply[M[_]: Monad, S: LooksAheadIn[M], V](
        builder: SimpleReader.Builder[V]
      )(using
        errs: Errors[M, S]
      ): SimpleReader[M, S, V] =
    new SimpleReader(builder, errs)
}
