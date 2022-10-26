package io.github.maxkar
package json.attr

import fun.typeclass.Applicative
import fun.typeclass.Monad

import text.input.LookAheadStream

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

import scala.collection.mutable.HashMap



/** Reader for the attributed json model. */
object Reader:
  /**
   * (Additional) error types thar are specific to this reader.
   * @tparam M execution monad.
   * @tparam S type of the input stream supported.
   * @tparam A type of the attributes supported by this error handler.
   */
  trait Errors[M[_], -S, -A]:
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
  end Errors


  object Errors:
    /**
     * Creates an error handler that ignores all the errors.
     * @param success value that denotes success in the given monad/applicative.
     */
    def ignoreBy[M[_]](success: M[Unit]): Errors[M, Any, Any] =
      new Errors[M, Any, Any]:
        override def duplicateObjectKey(prevEntry: Json.ObjectEntry[Any], newKeyAttrs: Any, stream: Any): M[Unit] =
          success
      end new


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
      new Reader.Errors[M, S, Any]:
        override def duplicateObjectKey(prevEntry: Json.ObjectEntry[Any], newKeyAttrs: Any, stream: S): M[Unit] =
          handler.raise(
            stream,
            s"Duplicate object entry with key '${prevEntry.key}'"
          )
      end new
    end simple
  end Errors



  /**
   * Reads a simple value from the stream and stops after the value was read.
   *
   * @param stream data stream to read.
   * @param attributeFactory factory used to create JSON attributes from data
   *   available through the given stream.
   */
  def readOneValue[M[_]: Monad, S <: LookAheadStream[M], A](
        stream: S,
        attributeFactory: AttributeFactory[M, S, A]
      )(using
        errs: Values.AllErrors[M, S],
        attrErrors: Errors[M, S, A]
      ): M[Json[A]] =
    import errs.given

    object reader extends Values.ValueCallback[M[Json[A]]]:

      override def onTrue(): M[Json[A]] =
        for {
          ctx <- attributeFactory.start(stream)
          _ <- Literals.readTrue(stream)
          attr <- attributeFactory.end(ctx, stream)
        } yield Json.True(attr)
      end onTrue


      override def onFalse(): M[Json[A]] =
        for {
          ctx <- attributeFactory.start(stream)
          _ <- Literals.readFalse(stream)
          attr <- attributeFactory.end(ctx, stream)
        } yield Json.False(attr)
      end onFalse


      override def onNull(): M[Json[A]] =
        for {
          ctx <- attributeFactory.start(stream)
          _ <- Literals.readNull(stream)
          attr <- attributeFactory.end(ctx, stream)
        } yield Json.Null(attr)
      end onNull


      override def onNumber(): M[Json[A]] =
        for {
          ctx <- attributeFactory.start(stream)
          repr <- Numbers.readAll(stream)
          attr <- attributeFactory.end(ctx, stream)
        } yield Json.Number(repr, attr)
      end onNumber


      override def onString(): M[Json[A]] =
        for {
          ctx <- attributeFactory.start(stream)
          value <- Strings.readAll(stream)
          attr <- attributeFactory.end(ctx, stream)
        } yield Json.String(value, attr)
      end onString


      override def onArray(): M[Json[A]] =
        for {
          ctx <- attributeFactory.start(stream)
          elements <-
            Arrays.readAll(
              skipWhitespaces = Whitespaces.skipAll[M],
              readValue = readValue,
              stream = stream
            )
          attr <- attributeFactory.end(ctx, stream)
        } yield Json.Array(elements, attr)
      end onArray


      override def onObject(): M[Json[A]] =
        for {
          ctx <- attributeFactory.start(stream)
          _ <- Objects.readObjectStart(stream)
          _ <- Whitespaces.skipAll(stream)
          nonEmpty <- Objects.hasFirstValue(stream)
          elems <-
            if nonEmpty then
              readObjectElements(stream, new HashMap())
            else
              Monad.pure(Map.empty)
          attr <- attributeFactory.end(ctx, stream)
        } yield Json.Object(elems, attr)
      end onObject


      /**
       * Reads object elemenst (key-value pairs) into the provided accumulator.
       */
      private def readObjectElements(
            stream: S,
            agg: HashMap[String, Json.ObjectEntry[A]],
          ): M[Map[String, Json.ObjectEntry[A]]] =
        for {
          _ <- Whitespaces.skipAll(stream)
          keyCtx <- attributeFactory.start(stream)
          key <- Strings.readAll(stream)
          keyAttrs <- attributeFactory.end(keyCtx, stream)
          _ <- {
            agg.get(key) match {
              case None => Monad.pure(())
              case Some(entry) => attrErrors.duplicateObjectKey(entry, keyAttrs, stream)
            }
          }
          _ <- Whitespaces.skipAll(stream)
          _ <- Objects.readKeyValueSeparator(stream)
          _ <- Whitespaces.skipAll(stream)
          value <- readValue(stream)
          _ = agg.put(key, Json.ObjectEntry(key, keyAttrs, value))
          _ <- Whitespaces.skipAll(stream)
          hasNext <- Objects.hasNextValue(stream)
          res <-
            if hasNext then
              readObjectElements(stream, agg)
            else
              Monad.pure(agg.toMap)
            end if
        } yield
          res
      end readObjectElements


      /** Reads a value. */
      def readValue(stream: S): M[Json[A]] =
        Monad.flatten(Values.expectedValue(stream, this, errs.valueErrors.illegalValue(stream)))
    end reader

    Whitespaces.skipAll(stream) flatMap { _ => reader.readValue(stream) }
  end readOneValue


  /**
   * Reads value from the stream ensuring that no other data is contained in
   * the `stream`. In other words, it reads the **whole** stream as a single
   * JSON value.
   *
   * @param stream data stream to read.
   * @param attributeFactory factory used to create JSON attributes from data
   *   available through the given stream.
   */
  def read[M[_]: Monad, S <: LookAheadStream[M], A](
        stream: S,
        attributeFactory: AttributeFactory[M, S, A]
      )(using
        errs: Values.AllErrors[M, S],
        attrErrors: Errors[M, S, A],
        eofErrors: EndOfFile.Errors[M, S],
      ): M[Json[A]] =
    for {
      res <- readOneValue(stream, attributeFactory)
      _ <- EndOfFile.expectNoValues(stream)
    } yield res
  end read

end Reader
