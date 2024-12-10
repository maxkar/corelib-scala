package io.github.maxkar
package json.classic

import implicits.given

import scala.language.implicitConversions


/** Tests for json serialization/parsing. */
final class JsonOutputTest extends org.scalatest.funsuite.AnyFunSuite {
  /** Serializes and re-parses json. */
  private def roll(v: Json): Json = Json.parse(Json.toString(v))


  test("Basic JSON serialization") {
    val x = Json.make("a" -> 3, "b" -> "test")
    val xx = roll(x)

    assert(3 === xx.a.as[Int])
    assert("test" === xx.b.as[String])
  }


  test("Conversion of optional values and nulls") {
    val notSet: Option[Int] = None
    val someSet: Option[Int] = Some(443)
    val x = Json.make("b" -> Json.Null, "c" -> notSet, "d" -> someSet)

    val xx = roll(x)
    assert(Json.Null === xx.b)
    assert(Json.Undefined === x.c)
    assert(443 === x.d.as[Int])
  }


  test("Json array conversions") {
    val a = Json.make("a" -> 5)
    val b: Json = 3
    val x = Json.make("arr" -> Json.array(a, b))

    val xx = roll(x)
    val arr: Seq[Json] = xx.arr

    assert(5 === arr(0).a.as[Int])
    assert(3 === arr(1).as[Int])
  }
}
