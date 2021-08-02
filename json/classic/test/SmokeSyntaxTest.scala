package io.github.maxkar
package json.classic

import implicits.given

import scala.language.implicitConversions

/** A short syntax test. */
final class SmokeSyntaxTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Simple syntax could be used to create json objects") {
    val x = Json.make("a" -> 3, "b" -> "test")
  }


  test("Reads from objects are convertible to proper types") {
    val obj = Json.make("a" -> 3)
    val v: Int = obj.a
    assert(3 === v)
  }


  test("Values are converted on call") {
    def fn(a: Int) = a
    val obj = Json.make("a" -> 3)

    assert(3 === fn(obj.a))
  }


  test("Options are converted on call") {
    def fn(a: Option[Int]) = a
    val obj = Json.make("a" -> 3)

    assert(None === fn(obj.xyz))
    assert(Some(3) === fn(obj.a))
  }


  test("Undefineds are not present on objects") {
    val obj = Json.make("a" -> Json.Undefined)
    val wals = obj.as[Json.Object].values
    assert(!wals.contains("a"))
  }


  test("Nested arrays are converted successfully") {
    val base = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
    val jsonArr: Json = base
    val recovered: Seq[Seq[Int]] = jsonArr

    assert(base === recovered)
  }


  test("Conversions works with maps") {
    val base = Map(
      "a" -> Map("x" -> 1, "y" -> 2), 
      "b" -> Map("x" -> 4, "y" -> 5)
    )

    val jsonMap: Json = base
    val recovered: Map[String, Map[String, Int]] = jsonMap

    assert(base === recovered)
  }


  test("Mixing all things works") {
    val obj = Json.make(
      "a" -> None,
      "b" -> Seq(Json.make("a" -> 5), Json.make("b" -> 4))
    )
  }
end SmokeSyntaxTest
