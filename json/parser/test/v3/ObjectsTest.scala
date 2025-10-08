package io.github.maxkar
package json.parser.v3

import TestIO.*


final class ObjectsTest extends org.scalatest.funsuite.AnyFunSuite {
  import ObjectsTest.*

  test("Happy path scenarios") {
    val data = Seq(
      """{}""" -> Map(),
      """{"a":43}""" -> Map("a" -> "43"),
      """{"a":43,"b":48,"c":54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{ "a":43,"b":48,"c":54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{"a":43,"b":48,"c":54 }""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{"a": 43, "b": 48, "c": 54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{"a" :43 ,"b" :48 ,"c" :54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{ "a" : 43 , "b" : 48 , "c" : 54 }""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
    )

    for
      (inputBase, expected) <- data
      rpad <- Seq.tabulate(5) { x => "X" * x }
      inputString = inputBase + rpad
    do
      withClue(inputString) {
        assert(expected === parse(inputString, Objects.read(_, Factory))._1)
      }
  }


  test("Object format errors") {
    val data = Seq(
      ("45", 0, "Invalid object start"),
      ("""{"a" -> 45}""", 5, "Invalid key-value separator"),
      ("""{"a"?45}""", 4, "Invalid key-value separator"),
      ("""{"a": 45, "b" -> 66}""", 14, "Invalid key-value separator"),
      ("""{"a":45+"b" -> 66}""", 7, "Invalid entry separator or object end"),
      ("""{"a": 45 +"b" -> 66}""", 9, "Invalid entry separator or object end"),
    )

    for
      (inputString, offset, message) <- data
    do
      withClue(inputString) {
        val actualExn = failParse(inputString, Objects.read(_, Factory))
        assert(offset === actualExn.offset)
        assert(message === actualExn.message)
      }
  }
}

object ObjectsTest {
  object Factory extends Objects.Factory[Operation, JsonStream, Map[String, String]] {
    override type Context = scala.collection.mutable.HashMap[String, String]
    override type Key = String

    override def skipIgnorableWhitespaces(stream: JsonStream): Operation[Unit] =
      Whitespaces.skip(stream)

    override def start(stream: JsonStream, count: Int): Operation[Context] =
      stream.skip(count) >-| new Context()

    override def invalidObjectStart(stream: JsonStream): Operation[Map[String, String]] =
      raise(stream, "Invalid object start")

    override def readKey(stream: JsonStream, context: Context): Operation[Key] =
      Strings.read(stream, StringsTest.Factory)

    override def skipKeyValueSeparator(
          stream: JsonStream,
          context: Context,
          key: String,
          count: Int
        ): Operation[Unit] =
      stream.skip(1)

    override def invalidKeyValueSeparator(
          stream: JsonStream,
          context: Context,
          key: String
        ): Operation[Unit] =
      raise(stream, "Invalid key-value separator")

    override def readValue(
          stream: JsonStream,
          context: Context,
          key: Key
        ): Operation[Unit] =
      Numbers.read(stream, NumbersTest.Factory) >-> { value =>
        context += (key -> value)
      }

    override def skipEntrySeparator(
          stream: JsonStream,
          context: Context,
          count: Int
        ): Operation[Unit] =
      stream.skip(count)


    override def finish(
          stream: JsonStream,
          context: Context,
          count: Int
        ): Operation[Map[String, String]] =
      stream.skip(count) >-| context.toMap


    override def invalidEntrySeparatorOrObjectEnd(
          stream: JsonStream,
          context: Context
        ): Operation[Map[String, String]] =
      raise(stream, "Invalid entry separator or object end")
  }
}
