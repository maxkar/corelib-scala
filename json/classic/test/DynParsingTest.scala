package io.github.maxkar
package json.classic

import implicits.given

import scala.language.implicitConversions


/** Tests for dynamic parsing syntax. */
final class DynParsingTest extends org.scalatest.funsuite.AnyFunSuite {
  /** Just a class for some syntax test. */
  case class T1(x: Int, y: String)

  /** Encodings for sniffing. */
  private val SNIFF_ENCODINGS = Seq("UTF-8", "UTF-16BE", "UTF-16LE", "UTF-32BE", "UTF-32LE")


  test("Basic object parsing and type conversions") {
    val json = Json.parse("""{"a": 3, "b": "aoeu"}""")
    val a: Int = json.a
    val b: String = json.b

    assert(3 === a)
    assert("aoeu" === b)
  }


  test("Json access expressions work in argument position") {
    val json = Json.parse("""{"a": 5, "b": "test"}""")
    val r = T1(json.a, json.b)

    assert(5 === r.x)
    assert("test" === r.y)
  }


  test("Parsing sequences works") {
    val json = Json.parse("""{"a": [1, 2, 5, 4]}""")
    val r: Seq[Int] = json.a

    assert(Seq(1, 2, 5, 4) === r)
  }


  test("There is a difference between explicit null and undefined") {
    val json = Json.parse("""{"a": null}""")

    assert(Json.Null === json.a)
    assert(Json.Undefined === json.b)
  }


  test("Minimal values works correctly with encoding detection") {
    val samples = Seq("1", "{}", "[]")
    for
      sample <- samples
      encoding <- SNIFF_ENCODINGS
    do
      Json.parseRFC7158(sample.getBytes(encoding))
  }
}
