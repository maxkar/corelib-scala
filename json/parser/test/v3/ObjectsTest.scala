package io.github.maxkar
package json.parser.v3

import TestIO.*
import TestIO.given

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
        assert(expected === parse(inputString, Objects.read(factory))._1)
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
        val actualExn = failParse(inputString, Objects.read(factory))
        assert(offset === actualExn.offset)
        assert(message === actualExn.message)
      }
  }
}

object ObjectsTest {
  val factory = new Objects.Factory.AsMap[Operation, JsonStream, String, String](
    Strings.read(StringsTest.factory),
    Numbers.read(NumbersTest.factory)
  )
}
