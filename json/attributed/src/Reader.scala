package io.github.maxkar
package json.attr

import fun.typeclass.Applicative
import fun.typeclass.Monad

import text.input.LookAheadStream
import text.v2.input.LooksAheadIn

import json.attr.Json.ObjectEntry

import json.parser.Values
import json.parser.Literals
import json.parser.Numbers
import json.parser.Strings
import json.parser.Objects
import json.parser.Arrays
import json.parser.Whitespaces
import json.parser.Values
import json.parser.EndOfFile
import json.parser.{Errors => StdErrors}

import json.parser.v2.ArrayReader
import json.parser.v2.SimpleReader
import json.parser.v2.StringReader
import json.parser.v2.LiteralReader
import json.parser.v2.NumberReader
import json.parser.v2.ObjectReader
import json.parser.v2.WhitespaceReader
import json.parser.v2.ValueReader

import scala.collection.mutable.HashMap


/**
 * Reader for the JSON with the specified attributes captured for each node.
 * @tparam M input/output monad
 * @tparam S type of the stream being read
 * @tparam A type of the attribute captured
 * @param attributeFactory a factory for capturing attributes from the stream.
 * @param attrErrors errors specific to this parser.
 * @param simpleErrors errors specific to most JSON parsers.
 */
final class Reader[M[_]: Monad, S: LooksAheadIn[M], A](
      attributeFactory: AttributeFactory[M, S, A],
      attrErrors: Reader.Errors[M, S, A],
      simpleErrors: SimpleReader.Errors[M, S]
    ) extends ValueReader.ValueReader[S, M[Json[A]]] {
  import simpleErrors.given

  private val literalReader = LiteralReader.all()

  def skipWhitespaces(stream: S): M[Unit] =
    WhitespaceReader(stream).skipAll()


  override def readTrue(stream: S): M[Json[A]] =
    for {
      ctx <- attributeFactory.start(stream)
      _ <- literalReader.trueLiteral(stream)
      attr <- attributeFactory.end(ctx, stream)
    } yield Json.True(attr)


  override def readFalse(stream: S): M[Json[A]] =
    for {
      ctx <- attributeFactory.start(stream)
      _ <- literalReader.falseLiteral(stream)
      attr <- attributeFactory.end(ctx, stream)
    } yield Json.False(attr)


  override def readNull(stream: S): M[Json[A]] =
    for {
      ctx <- attributeFactory.start(stream)
      _ <- literalReader.nullLiteral(stream)
      attr <- attributeFactory.end(ctx, stream)
    } yield Json.Null(attr)


  override def readString(stream: S): M[Json[A]] =
    for {
      ctx <- attributeFactory.start(stream)
      str <- StringReader(stream).readString()
      attr <- attributeFactory.end(ctx, stream)
    } yield Json.String(str, attr)


  override def readNumber(stream: S): M[Json[A]] =
    for {
      ctx <- attributeFactory.start(stream)
      str <- NumberReader(stream).readString()
      attr <- attributeFactory.end(ctx, stream)
    } yield Json.Number(str, attr)


  override def readArray(stream: S): M[Json[A]] =
    for {
      ctx <- attributeFactory.start(stream)
      elements <- ArrayReader(stream, skipWhitespaces).readSequence(readValue)
      attr <- attributeFactory.end(ctx, stream)
    } yield Json.Array(elements, attr)


  override def readObject(stream: S): M[Json[A]] =
    for {
      ctx <- attributeFactory.start(stream)
      elements <- readObjectMap(stream)
      attr <- attributeFactory.end(ctx, stream)
    } yield Json.Object(elements, attr)


  /** Reads a single value from the stream. */
  def readValue(stream: S): M[Json[A]] =
    skipWhitespaces(stream) <+> ValueReader.readValue(stream, this)


  /** Reads the value and ensures there is no other values in the stream. */
  def readFully(stream: S): M[Json[A]] =
    for {
      res <- readValue(stream)
      _ <- skipWhitespaces(stream)
      _ <- ensureAtEnd(stream)
    } yield res


  /** Reads object's key-value map. */
  private def readObjectMap(stream: S): M[Map[String, ObjectEntry[A]]] = {
    val agg = new scala.collection.mutable.HashMap[String, ObjectEntry[A]]()
    val objReader = ObjectReader(stream, skipWhitespaces)
    def step(): M[Map[String, ObjectEntry[A]]] = {
      objReader.advanceToNext() <||| {
        case true =>
          for {
            keyCtx <- attributeFactory.start(stream)
            key <- StringReader(stream).readString()
            keyAttrs <- attributeFactory.end(keyCtx, stream)
            _ <- objReader.readKeyValueSeparator()
            value <- readValue(stream)
            _ <- agg.get(key) match {
              case None =>
                agg.put(key, ObjectEntry(key, keyAttrs, value))
                Monad.pure(())
              case Some(prevValue) =>
                attrErrors.duplicateObjectKey(prevValue, keyAttrs, stream)
            }
            res <- step()
          } yield
            res
        case false => Monad.pure(agg.toMap)
      }
    }
    step()
  }


  /** Checks if the stream is at the end. */
  private def ensureAtEnd(stream: S): M[Unit] =
    stream.atEnd() <| {
      case true => Monad.pure(())
      case false => simpleErrors.trailingData(stream)
    }
}

/** Reader for the attributed json model. */
object Reader {
  /**
   * (Additional) error types thar are specific to this reader.
   * @tparam M execution monad.
   * @tparam S type of the input stream supported.
   * @tparam A type of the attributes supported by this error handler.
   */
  trait Errors[M[_], -S, -A] {
    /**
     * Handles a situation where json object contains duplicate keys. An implementation
     * may decide to flag a error and abort execution in monad-specific way. If the
     * execution completes successfully, new value would be read and the new entry will
     * replace the old one. In other words, the latest value observed takes the precedence
     * in case of successfull execution of this method.
     *
     * @param prevEntry previous entry with the same name (both key
     *   and value with their respective attributes).
     * @param newKeyAttrs attributes that are to be applied to the new key.
     * @param stream stream that contained the duplicate information.
     *   Unlike with many other error handlers, stream position is **after**
     *   the duplicate key.
     * @return result of handling the duplicate key situation.
     */
    def duplicateObjectKey(prevEntry: Json.ObjectEntry[A], newKeyAttrs: A, stream: S): M[Unit]
  }


  object Errors {
    /**
     * Creates an error handler that ignores all the errors.
     * @param success value that denotes success in the given monad/applicative.
     */
    def ignoreBy[M[_]](success: M[Unit]): Errors[M, Any, Any] =
      new Errors[M, Any, Any] {
        override def duplicateObjectKey(prevEntry: Json.ObjectEntry[Any], newKeyAttrs: Any, stream: Any): M[Unit] =
          success
      }


    /** Creates an error handler that ignores all the errors. */
    def ignore[M[_]: Applicative]: Errors[M, Any, Any] = ignoreBy(Applicative.pure(()))


    /**
     * Creates a handler that just raises the error with human-readable message, similar
     * to stanadard hanlers. Note that error location reporting may be not very correct.
     *
     * @param handler the handler that knows how to encode the errorm into the
     *   execution monad M. It may also enrich the message with the error context
     *   (like location) from the input stream.
     */
    def simple[M[_]: Monad, S <: LookAheadStream[M]](handler: StdErrors.SimpleHandler[M, S]): Reader.Errors[M, S, Any] =
      new Reader.Errors[M, S, Any] {
        override def duplicateObjectKey(prevEntry: Json.ObjectEntry[Any], newKeyAttrs: Any, stream: S): M[Unit] =
          handler.raise(
            stream,
            s"Duplicate object entry with key '${prevEntry.key}'"
          )
      }


    /** Creates a simple error handler where errors are converted to text and raised in the monad. */
    def raise[M[_], S](raiseFn: [T] => (S, String) => M[T]): Errors[M, S, Any] =
      new Reader.Errors[M, S, Any] {
        override def duplicateObjectKey(prevEntry: ObjectEntry[Any], newKeyAttrs: Any, stream: S): M[Unit] =
          raiseFn(stream, s"Duplicate object entry with key '${prevEntry.key}'")
      }
  }



  /**
   * Reads a simple value from the stream and stops after the value was read.
   *
   * @param stream data stream to read.
   * @param attributeFactory factory used to create JSON attributes from data
   *   available through the given stream.
   */
  def readOneValue[M[_]: Monad, S: LooksAheadIn[M], A](
        stream: S,
        attributeFactory: AttributeFactory[M, S, A]
      )(using
        errs: SimpleReader.Errors[M, S],
        attrErrors: Errors[M, S, A]
      ): M[Json[A]] =
    new Reader(attributeFactory, attrErrors, errs).readValue(stream)


  /**
   * Reads value from the stream ensuring that no other data is contained in
   * the `stream`. In other words, it reads the **whole** stream as a single
   * JSON value.
   *
   * @param stream data stream to read.
   * @param attributeFactory factory used to create JSON attributes from data
   *   available through the given stream.
   */
  def read[M[_]: Monad, S: LooksAheadIn[M], A](
        stream: S,
        attributeFactory: AttributeFactory[M, S, A]
      )(using
        errs: SimpleReader.Errors[M, S],
        attrErrors: Errors[M, S, A]
      ): M[Json[A]] =
    new Reader(attributeFactory, attrErrors, errs).readFully(stream)
}
