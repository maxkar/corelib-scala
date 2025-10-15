package io.github.maxkar
package json.simple

import fun.typeclass.Monad

import json.parser.Peek
import json.parser.DefaultStream
import json.parser.Literals
import json.parser.Strings
import json.parser.Numbers
import json.parser.Arrays
import json.parser.Objects
import json.parser.Values
import json.parser.Whitespaces
import json.parser.ParseError

final class Reader[M[_]: Monad, -S: Peek.In[M]: DefaultStream.In[M]: ParseError.In[M]] {
  /** Factory for the literal values. */
  private val readTrueFn = Literals.readTrue(new Literals.Factory.Simple(Json.True: Json))
  private val readFalseFn = Literals.readFalse(new Literals.Factory.Simple(Json.False: Json))
  private val readNullFn = Literals.readNull(new Literals.Factory.Simple(Json.Null: Json))

  private val readStringFn = Strings.read(new Strings.Factory.AsString())
  private val readNumberFn = Numbers.read(new Numbers.Factory.AsString())
  private val readArrayFn = Arrays.read(new Arrays.Factory.AsSequence(readValue))
  private val readObjectFn = Objects.read(new Objects.Factory.AsMap(readStringFn, readValue))

  /** Factory for values. */
  private val valueFactory =
    new Values.Factory[S, M[Json]] {
      override def readTrue(stream: S): M[Json] =
        readTrueFn(stream)
      override def readFalse(stream: S): M[Json] =
        readFalseFn(stream)
      override def readNull(stream: S): M[Json] =
        readNullFn(stream)
      override def readString(stream: S): M[Json] =
        readStringFn(stream) >-> Json.String.apply
      override def readNumber(stream: S): M[Json] =
        readNumberFn(stream) >-> Json.Number.apply
      override def readArray(stream: S): M[Json] =
        readArrayFn(stream) >-> Json.Array.apply
      override def readObject(stream: S): M[Json] =
        readObjectFn(stream) >-> Json.Object.apply
      override def invalidValue(stream: S): M[Json] =
        stream.parseError("Invalid JSON value")
    }

  /** Reads one value from the stream. */
  def readValue(stream: S): M[Json] =
    Values.read(valueFactory)(stream)


  def readFully(stream: S): M[Json] =
    Values.readFully(valueFactory)(stream)
}
