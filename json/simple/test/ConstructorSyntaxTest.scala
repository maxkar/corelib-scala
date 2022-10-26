package io.github.maxkar
package json.simple

import defaultConversions.given

import scala.language.implicitConversions

/**
 * Tests for simple json construction syntax.
 */
final class ConstructorSyntaxTest extends org.scalatest.funsuite.AnyFunSuite:

  test("Smoke construction test") {
    check(Json.True, true)
    check(Json.False, false)
    check(Json.String("Hello, Scala!"), "Hello, Scala!")
    check(Json.Number("123"), 123)
    check(Json.Number("123"), 123L)
    check(Json.Number("123.5"), 123.5f)
    check(Json.Number("123.5"), 123.5)
    check(Json.Number("213"), BigInt("213"))
    check(Json.Number("213.25"), BigDecimal("213.25"))
  }


  test("Array construction and manipulation syntax") {
    check(Json.Array(Seq(Json.True, Json.False)), Json.array(true, false))
    check(Json.Array(Seq(Json.True)), Json.array(true, Json.Empty))
    check(Json.Array(Seq(Json.False)), Json.array(Json.Empty, false))

    val baseSeq = Seq(Json.Number("42"))
    val base = Json.array(42)

    check(Json.Array(baseSeq), base)
    check(Json.Array(baseSeq ++ baseSeq), base.update(42))
    check(Json.Array(baseSeq :+ Json.Number("44")), base.update(Json.Empty, 44))

    val deepArray =
      Json.Array(Seq(
        Json.Array(Seq(
          Json.True, Json.False
        ))
      ))

    check(deepArray, Seq(Seq(true, false)))

    check(deepArray, Seq(Seq(Some(true), Some(false), None)))
    check(deepArray, Seq(Seq(Some(true), None, Some(false), None)))
    check(deepArray, Seq(Some(Seq(Some(true), None, Some(false), None)), None))
  }


  test("Object construction and manipulation syntax") {
    check(
      Json.Object(Map("a" -> Json.True, "b" -> Json.False)),
      Json.make("a" -> true, "b" -> false)
    )

    check(
      Json.Object(Map("a" -> Json.True, "b" -> Json.String("false"))),
      Json.make("a" -> true, "b" -> "false")
    )

    check(
      Json.Object(Map("a" -> Json.True, "b" -> Json.String("false"))),
      Json.make("a" -> true, "b" -> "false", "c" -> (None: Option[String]))
    )

    val baseMap = Map("a" -> Json.True, "b" -> Json.Number("42"))
    val base = Json.make("a" -> true, "b" -> 42)

    check(Json.Object(baseMap), base)

    check(Json.Object(baseMap + ("c" -> Json.False)), base.update("c" -> false))
    check(Json.Object(baseMap + ("c" -> Json.False)), base.update("c" -> Some(false)))
    check(Json.Object(baseMap + ("c" -> Json.False)), base.update("c" -> Some(false)))
    check(Json.Object(baseMap), base.update("c" -> (None: Option[Boolean])))
    check(Json.Object(baseMap - "a"), base.update("a" -> Json.Remove))
    check(Json.Object(baseMap - "a" + ("c" -> Json.True)), base.update("a" -> Json.Remove, "c" -> true))

    val deepObject =
      Json.Object(Map(
        "a" -> Json.Object(Map(
          "b" -> Json.True,
          "c" -> Json.False,
        )),
        "d" -> Json.Object(Map(
          "e" -> Json.False
        ))
      ))

    check(
      deepObject,
      Map(
        "a" -> Map("b" -> true, "c" -> false),
        "d" -> Map("e" -> false)
      )
    )

    check(
      deepObject,
      Map(
        "a" -> Map("b" -> Some(true), "c" -> Some(false)),
        "d" -> Map("e" -> Some(false), "f" -> None)
      )
    )
  }


  test("Combining optionals, map and array conversions in one place works") {
    val x: Seq[Option[Map[String, Option[Boolean]]]] =
      Seq(
        Some(Map(
          "a" -> Some(true),
          "b" -> None,
          "c" -> Some(false)
        )),
        None,
        None,
        Some(Map(
          "e" -> None,
          "f" -> Some(true)
        ))
      )

    val expected =
      Json.Array(Seq(
        Json.Object(Map(
          "a" -> Json.True,
          "c" -> Json.False,
        )),
        Json.Object(Map(
          "f" -> Json.True
        )),
      ))

    check(expected, x)
  }


  /** Checks that x and y match. Most useful for implicit value conversion tests. */
  private def check(x: Json, y: Json): Unit =
    assert(x === y)
end ConstructorSyntaxTest
