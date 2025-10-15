package io.github.maxkar
package json.parser

import fun.typeclass.Monad
import TestIO.*

final class ValuesTest extends org.scalatest.funsuite.AnyFunSuite {
  import ValuesTest.*

  test("Basic smoke tests") {
    val data = Seq(
      "true" -> "true",
      "false" -> "false",
      "645.5" -> "number",
      "-645.5" -> "number",
      "[]" -> "array",
      "null" -> "null",
      "{}" -> "object",
      "\"Hello\"" -> "string",
    )

    for
      (inputString, expected) <- data
    do
      withClue(inputString) {
        assert(expected === parse(inputString, Values.read(Factory))._1)
      }
  }


  test("Some basic errors tests") {
    val exn = failParse("xref", Values.read(Factory))
    assert(0 === exn.offset)
  }
}

object ValuesTest {
  object Factory extends Values.Factory[JsonStream, Operation[String]] {
    override def readTrue(stream: JsonStream): Operation[String] =
      Monad.pure("true")
    override def readFalse(stream: JsonStream): Operation[String] =
      Monad.pure("false")
    override def readNull(stream: JsonStream): Operation[String] =
      Monad.pure("null")
    override def readNumber(stream: JsonStream): Operation[String] =
      Monad.pure("number")
    override def readString(stream: JsonStream): Operation[String] =
      Monad.pure("string")
    override def readObject(stream: JsonStream): Operation[String] =
      Monad.pure("object")
    override def readArray(stream: JsonStream): Operation[String] =
      Monad.pure("array")
    override def invalidValue(stream: JsonStream): Operation[String] =
      raise(stream, "Invalid value")
  }
}
