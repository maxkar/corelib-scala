package io.github.maxkar
package json.attr

import fun.typeclass.Monad

import json.parser.v3.Peek
import json.parser.v3.DefaultStream
import json.parser.v3.Literals
import json.parser.v3.Strings
import json.parser.v3.Numbers
import json.parser.v3.Arrays
import json.parser.v3.Objects
import json.parser.v3.Values
import json.parser.v3.Whitespaces
import io.github.maxkar.json.parser.v3.Whitespaces.skip

/** A reader of the input stream that has default capabilities. */
final class Reader[M[_]: Monad, -S: Peek.In[M]: DefaultStream.In[M], A](
      factory: Reader.Factory[M, S, A]
    ) {


  /** Factory for the literal values. */
  private val literalFactory =
    new Literals.Factory[M, S, Unit] {
      override def read(stream: S, count: Int): M[Unit] =
        stream.skip(count)

      override def invalidLiteral(stream: S, expected: String): M[Unit] =
        parseError(stream, s"Invalid ${expected} literal")
    }


  /** Factory for string values. */
  private val stringFactory =
    new Strings.Factory[M, S, String] {
      override type Context = StringBuilder

      override def start(stream: S, count: Int): M[Context] =
        stream.skip(count) >-| new Context()

      override def invalidStringStart(stream: S): M[Context] =
        parseError(stream, "Invalid string start")

      override def readWhile(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        stream.readWhile(context, predicate)

      override def readEscape(stream: S, context: Context, count: Int, char: Char): M[Unit] = {
        context += char
        stream.skip(count)
      }

      override def invalidEscapeCharacter(stream: S, context: Context): M[Unit] =
        parseError(stream, "Invalid escape character")

      override def invalidUnicodeEscape(stream: S, context: Context): M[Unit] =
        parseError(stream, "Invalid unicode escape")

      override def invalidCharacter(stream: S, context: Context): M[Unit] =
        parseError(stream, "Invalid character")

      override def finish(stream: S, context: Context, count: Int): M[String] =
        stream.skip(count) >-| context.toString()

      override def invalidStringEnd(stream: S, context: Context): M[String] =
        parseError(stream, "Invalid string end")
    }


  /** Factory for numeric values. */
  private val numberFactory =
    new Numbers.Factory[M, S, String] {
      override type Context = StringBuilder

      override def start(stream: S): M[Context] = Monad.pure(new Context())

      override def readSign(stream: S, context: Context, count: Int, sign: Char): M[Unit] = {
        context += sign
        stream.skip(count)
      }

      override def readIntegerDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        stream.readWhile(context, predicate)

      override def missingIntegerDigits(stream: S, context: Context): M[Unit] =
        parseError(stream, "Missing integer digits")

      override def leadingIntegerZero(stream: S, context: Context): M[Unit] =
        parseError(stream, "Leading 0 is not allowed")

      override def readDecimalSeparator(stream: S, context: Context, count: Int, separator: Char): M[Unit] = {
        context += separator
        stream.skip(count)
      }

      override def readDecimalDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        stream.readWhile(context, predicate)

      override def missingDecimalDigits(stream: S, context: Context): M[Unit] =
        parseError(stream, "Missing decimal digits")

      override def readExponentIndicator(stream: S, context: Context, count: Int, separator: Char): M[Unit] = {
        context += separator
        stream.skip(count)
      }

      override def readExponentSign(stream: S, context: Context, count: Int, separator: Char): M[Unit] = {
        context += separator
        stream.skip(count)
      }

      override def readExponentDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        stream.readWhile(context, predicate)

      override def missingExponentDigits(stream: S, context: Context): M[Unit] =
        parseError(stream, "Missing exponent digits")

      override def finish(stream: S, context: Context): M[String] = Monad.pure(context.toString())
    }


  /** Factory for arrays. */
  private val arrayFactory =
    new Arrays.Factory[M, S, Seq[Json[A]]] {
      override type Context = scala.collection.mutable.ArrayBuffer[Json[A]]

      override def skipIgnorableWhitespaces(stream: S): M[Unit] =
        skipWhitespaces(stream)

      override def start(stream: S, count: Int): M[Context] =
        stream.skip(count) >-| new Context()

      override def invalidArrayStart(stream: S): M[Seq[Json[A]]] =
        parseError(stream, "Invalid array start")

      override def readValue(stream: S, context: Context): M[Unit] =
        Reader.this.readValue(stream) >-> context.append

      override def skipValueSeparator(stream: S, context: Context, count: Int): M[Unit] =
        stream.skip(1)

      override def invalidValueSeparatorOrArrayEnd(stream: S, context: Context): M[Seq[Json[A]]] =
        parseError(stream, "Invalid value separator or array end")

      override def finish(stream: S, context: Context, count: Int): M[Seq[Json[A]]] =
        stream.skip(count) >-| context.toSeq
    }


  /** Factory for objects. */
  private val objectFactory =
    new Objects.Factory[M, S, Map[String, Json.ObjectEntry[A]]] {
      override type Context = scala.collection.mutable.HashMap[String, Json.ObjectEntry[A]]
      override type Key = (Json[A] => Json.ObjectEntry[A])

      override def skipIgnorableWhitespaces(stream: S): M[Unit] =
        skipWhitespaces(stream)

      override def start(stream: S, count: Int): M[Context] =
        stream.skip(count) >-| new Context()

      override def invalidObjectStart(stream: S): M[Map[String, Json.ObjectEntry[A]]] =
        parseError(stream, "Invalid object start")

      override def readKey(stream: S, context: Context): M[Key] =
        readWithAttr(stream, Strings.read(stream, stringFactory)) >=>> { (attr, key) =>
          context.get(key) match {
            case Some(prev) => factory.duplicateObjectKey(prev, attr, stream)
            case None => Monad.pure(Json.ObjectEntry(key, attr, _))
          }
        }

      override def skipKeyValueSeparator(stream: S, context: Context, key: Key, count: Int): M[Unit] =
        stream.skip(count)

      override def invalidKeyValueSeparator(stream: S, context: Context, key: Key): M[Unit] =
        parseError(stream, "Invalid key-value separator")

      override def readValue(stream: S, context: Context, key: Key): M[Unit] =
        Reader.this.readValue(stream) >-> { value =>
          val entry = key(value)
          context.put(entry.key, entry)
        }

      override def skipEntrySeparator(stream: S, context: Context, count: Int): M[Unit] =
        stream.skip(count)

      override def finish(stream: S, context: Context, count: Int): M[Map[String, Json.ObjectEntry[A]]] =
        stream.skip(count) >-| context.toMap

      override def invalidEntrySeparatorOrObjectEnd(stream: S, context: Context): M[Map[String, Json.ObjectEntry[A]]] =
        parseError(stream, "Invalid entry separator or object end")
    }

  /** Factory for values. */
  private val valueFactory =
    new Values.Factory[S, M[Json[A]]] {
      override def readTrue(stream: S): M[Json[A]] =
        readJsonValue(stream, Literals.readTrue(stream, literalFactory), (_, attr) => Json.True(attr))

      override def readFalse(stream: S): M[Json[A]] =
        readJsonValue(stream, Literals.readFalse(stream, literalFactory), (_, attr) => Json.False(attr))

      override def readNull(stream: S): M[Json[A]] =
        readJsonValue(stream, Literals.readNull(stream, literalFactory), (_, attr) => Json.Null(attr))

      override def readString(stream: S): M[Json[A]] =
        readJsonValue(stream, Strings.read(stream, stringFactory), Json.String.apply)

      override def readNumber(stream: S): M[Json[A]] =
        readJsonValue(stream, Numbers.read(stream, numberFactory), Json.Number.apply)

      override def readArray(stream: S): M[Json[A]] =
        readJsonValue(stream, Arrays.read(stream, arrayFactory), Json.Array.apply)

      override def readObject(stream: S): M[Json[A]] =
        readJsonValue(stream, Objects.read(stream, objectFactory), Json.Object.apply)

      override def invalidValue(stream: S): M[Json[A]] =
        parseError(stream, "Invalid JSON value")
    }


  /** Reads one value from the stream. */
  def readValue(stream: S): M[Json[A]] =
    skipWhitespaces(stream) >=|| Values.read(stream, valueFactory)


  def readFully(stream: S): M[Json[A]] =
    for
      res <- readValue(stream)
      _ <- skipWhitespaces(stream)
      _ <- stream.atEof() >-> { eof => if eof then Monad.pure(()) else factory.eofExpected(stream) }
    yield res


  /** Skips whitespaces in the stream. */
  def skipWhitespaces(stream: S): M[Unit] =
    Whitespaces.skip(stream)


  /** Shorthand for factory.parseError. */
  private def parseError[T](stream: S, message: String): M[T] =
    factory.parseError(stream, message)


  /** Reads value with its attribute. */
  private def readWithAttr[T](stream: S, block: => M[T]): M[(A, T)] =
    for
      ctx <- factory.start(stream)
      value <- block
      attr <- factory.finish(ctx, stream)
    yield
      (attr, value)


  /** Reads value with its attribute and constructs JSON. */
  private def readJsonValue[T](stream: S, block: => M[T], cb: (T, A) => Json[A]): M[Json[A]] =
    for
      ctx <- factory.start(stream)
      value <- block
      attr <- factory.finish(ctx, stream)
    yield
      cb(value, attr)
}


object Reader {
  /** Factory for the Json Attributes and handling errors. */
  trait Factory[M[_], -S, A] {
    /**
     * Type of the context being captured at the value start position. Values
     * of this type are used to carry-over some context until the value is
     * fully read.
     */
    type Context

    /**
     * Captures all the required information an the
     * **start** of some value in the source stream.
     * @param stream source data stream that could be used to extract information.
     * @return captured context that will later be passed to the `end` function.
     */
    def start(stream: S): M[Context]

    /**
     * Generates the attribute(s) based on the context (captured at the start
     * of the value) and the stream data after the value was read.
     * @param context context returned from the `start` method at the starting position.
     * @param stream source data stream that could be used to extract information.
     * @return attribute that should be applied to the value read between the
     * corresponding `start` and `end` calls.
     */
    def finish(context: Context, stream: S): M[A]


    /**
     * Raises an error for the given stream.
     * @param stream stream where the issue occured.
     * @param error error description.
     */
    def parseError[T](stream: S, error: String): M[T]


    /**
     * Handles a situation where a duplicate key was found in an object.
     * @param prevEntry previous entry with the same key.
     * @param newKeyAttrs attributes of the new key.
     * @param stream stream where the issue happened. The
     *   stream position is just after the key.
     * @return a "merge" function that takes a new value and returns
     *   the entry that should be a result. For example, a function
     *   may choose to use the _first_ value, the _last_ one or
     *   attempt to merge them.
     */
    def duplicateObjectKey(
          prevEntry: Json.ObjectEntry[A],
          newKeyAttrs: A,
          stream: S)
        : M[Json[A] => Json.ObjectEntry[A]]


    /** Hadles a condition where End-of-file(stream) was expected but more data was present. */
    def eofExpected(stream: S): M[Unit]
  }
}
