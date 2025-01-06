package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException

final class SimpleReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given
  import SimpleReaderTest.*
  import SimpleReaderTest.given

  private val jsonReader = new SimpleReader(TestValueBuilder, SimpleReaderTest.parseErrors)


  test("Basic smoke tests") {
    val data = Seq(
      "true" -> java.lang.Boolean.TRUE,
      "false" -> java.lang.Boolean.FALSE,
      "645.5" -> BigDecimal("645.5"),
      "-645.5" -> BigDecimal("-645.5"),
      "[]" -> Seq.empty,
      "null" -> null,
      "{}" -> Map.empty,
      "\"Hello\"" -> "Hello",
      """[null]""" -> Seq(null),
      """["Hello", "World" ]""" -> Seq("Hello", "World"),
      """["Hello", 44 ]""" -> Seq("Hello", BigDecimal("44")),
      """{"a": 22, "b": true}""" -> Map("a" -> BigDecimal("22"), "b" -> java.lang.Boolean.TRUE),
      """{"a": [true], "b": {"c": "42", "d": []}}""" ->
        Map("a" -> Seq(java.lang.Boolean.TRUE), "b" -> Map("c" -> "42", "d" -> Seq.empty)),
    )

    for
      (inputBase, expected) <- data
      lpad <- Seq.tabulate(5) { x => " " * x }
      rpad <- Seq.tabulate(5) { x => " " * x }
      inputString = lpad + inputBase + rpad
    do
      withClue(inputString) {
        assert(expected === read(inputString))
      }
  }


  test("Some basic errors tests") {
    val data = Seq(
      ("""xref""", 0),
      ("""[[], xref]""", 5),
      ("""[{,}]""", 2),
      ("""[{  ,}]""", 4),
      ("""[{  a:b}]""", 4),
      ("""[{  "a":b}]""", 8),
      ("""[{}""", 3),
    )

    for
      (inputString, offset) <- data
    do
      withClue(inputString) {
        val exn = intercept[Err] { read(inputString) }
        assert(offset === exn.offset)
      }
  }

  private def read(source: String): Object =
    Unnest.run(jsonReader.readValue(stringInput(source)))
}


object SimpleReaderTest {
  import TestIO.*
  import TestIO.given


  given parseErrors: SimpleReader.Errors[Unnest, IOStream] =
    SimpleReader.simpleErrors { [T] => (stream, message) =>
      stream.getLocation() <| { location =>
        throw new Err(location.offset, message)
      }
    }

  /** Simple model generator. */
  object TestValueBuilder extends SimpleReader.Builder[AnyRef] {
    override def fromNull(): AnyRef = null
    override def fromBoolean(v: Boolean): AnyRef = java.lang.Boolean.valueOf(v)
    override def fromNumber(repr: String): AnyRef = BigDecimal(repr)
    override def fromString(value: String): AnyRef = value
    override def fromArray(items: Seq[AnyRef]): AnyRef = items
    override def fromObject(items: Map[String, AnyRef]): AnyRef = items
  }

  final case class Err(offset: Int, message: String) extends IOException(s"${offset}:${message}")
}
