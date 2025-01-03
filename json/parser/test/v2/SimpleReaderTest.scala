package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException

final class SimpleReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import Unnest.given
  import SimpleReaderTest.*
  import SimpleReaderTest.given


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

  private def read(source: String): Object = {
    val sr = new java.io.StringReader(source): java.io.Reader
    val br = BufferedLookAhead(sr, 100)
    val jsonReader = new SimpleReader(TestValueBuilder, SimpleReaderTest.parseErrors)

    Unnest.run(jsonReader.readValue(br))
  }
}


object SimpleReaderTest {
  import Unnest.given
  type IOStream = BufferedLookAhead[java.io.Reader]

  private given unnestError: BufferedLookAhead.IOErrors[Unnest] with {
    override def lookAheadTooBig[T](requested: Int, supported: Int): Unnest[T] =
      throw new IOException(
        s"Look ahead ${requested} is greater than the supported amount of ${supported}"
      )
  }


  /** Reader for java instances. */
  private given javaReaderReader[M[_]: Monad, T <: java.io.Reader]: Reader[M, T] with {
    override def read(source: T, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      Monad.pure(source.read(target, targetStart, targetEnd - targetStart))
   }


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
