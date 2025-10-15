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
import json.parser.v3.ParseError

/** A reader of the input stream that has default capabilities. */
final class Reader[M[_]: Monad, -S: Peek.In[M]: DefaultStream.In[M], A](
      factory: Reader.Factory[M, S, A]
    ) {

  private given ParseError[M, S] with {
    extension (stream: S) {
      override def parseError[T](message: String): M[T] =
        factory.parseError(stream, message)
    }
  }

  /** Factory for the literal values. */
  private val literalFactory = new Literals.Factory.Simple(())
  private val readTrueFn = Literals.readTrue(literalFactory)
  private val readFalseFn = Literals.readFalse(literalFactory)
  private val readNullFn = Literals.readNull(literalFactory)

  /** Factory for string values. */
  private val stringFactory = new Strings.Factory.AsString()
  private val readStringFn = Strings.read(stringFactory)

  /** Factory for numeric values. */
  private val numberFactory = new Numbers.Factory.AsString()
  private val readNumberFn = Numbers.read(numberFactory)

  /** Factory for arrays. */
  private val arrayFactory = new Arrays.Factory.AsSequence(Reader.this.readValue)
  private val readArrayFn = Arrays.read(arrayFactory)

  /** Factory for objects. */
  private val objectFactory =
    new Objects.Factory.Simple[M, S, Map[String, Json.ObjectEntry[A]]] {
      override type Context = scala.collection.mutable.HashMap[String, Json.ObjectEntry[A]]
      override type Key = (Json[A] => Json.ObjectEntry[A])

      override def createContext(): Context = new Context()
      override def createValue(context: Context): Map[String, Json.ObjectEntry[A]] = context.toMap

      override def readKey(stream: S, context: Context): M[Key] =
        readWithAttr(stream, Strings.read(stringFactory)(stream)) >=>> { (attr, key) =>
          context.get(key) match {
            case Some(prev) => factory.duplicateObjectKey(prev, attr, stream)
            case None => Monad.pure(Json.ObjectEntry(key, attr, _))
          }
        }

      override def readValue(stream: S, context: Context, key: Key): M[Unit] =
        Reader.this.readValue(stream) >-> { value =>
          val entry = key(value)
          context.put(entry.key, entry)
        }
    }
  private val readObjectFn = Objects.read(objectFactory)

  /** Factory for values. */
  private val valueFactory =
    new Values.Factory[S, M[Json[A]]] {
      override def readTrue(stream: S): M[Json[A]] =
        readJsonValue(stream, readTrueFn, (_, attr) => Json.True(attr))

      override def readFalse(stream: S): M[Json[A]] =
        readJsonValue(stream, readFalseFn, (_, attr) => Json.False(attr))

      override def readNull(stream: S): M[Json[A]] =
        readJsonValue(stream, readNullFn, (_, attr) => Json.Null(attr))

      override def readString(stream: S): M[Json[A]] =
        readJsonValue(stream, readStringFn, Json.String.apply)

      override def readNumber(stream: S): M[Json[A]] =
        readJsonValue(stream, readNumberFn, Json.Number.apply)

      override def readArray(stream: S): M[Json[A]] =
        readJsonValue(stream, readArrayFn, Json.Array.apply)

      override def readObject(stream: S): M[Json[A]] =
        readJsonValue(stream, readObjectFn, Json.Object.apply)

      override def invalidValue(stream: S): M[Json[A]] =
        parseError(stream, "Invalid JSON value")
    }


  /** Reads one value from the stream. */
  def readValue(stream: S): M[Json[A]] =
    skipWhitespaces(stream) >=|| Values.read(valueFactory)(stream)


  def readFully(stream: S): M[Json[A]] =
    for
      res <- readValue(stream)
      _ <- skipWhitespaces(stream)
      _ <- stream.peek(0) >-> { next => if next < 0 then Monad.pure(()) else factory.eofExpected(stream) }
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
  private def readJsonValue[T](stream: S, body: S => M[T], cb: (T, A) => Json[A]): M[Json[A]] =
    for
      ctx <- factory.start(stream)
      value <- body(stream)
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
