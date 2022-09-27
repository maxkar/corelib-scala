package io.github.maxkar
package json.attr.query

import json.query.Query
import json.query.ModelNavigation
import json.attr.Json

import json.attr.query.given
import defaultConversions.given


/**
 * Tests for simple json conversion and navigation.
 */
final class ConversionAndSyntaxTest extends org.scalatest.funsuite.AnyFunSuite:
  given ConvertibleBy[({type Id[T] = T})#Id, Int] = ConvertibleBy.identityWithLocation(_.toString)

  test("Basic query and navigation on json works") {
    val model: Json[Int] =
      Json.Object(Map(
        "a" -> Json.ObjectEntry("a", 2, Json.True(1)),
        "b" -> Json.ObjectEntry("b", 4, Json.Number("23", 3)),
      ), 5)

    val q = Query(model)
    assert(q.b.as[Int] === 23)
    assert(q.a.as[Boolean] === true)
    assert(q.c.as[Option[String]] === None)

    val qm = q.as[Map[String, Query[Json[Int]]]]
    assert(qm("a").as[Boolean] === true)
  }


  test("Array derivations work") {
    val model1: Json[Int] = Json.Array(
      Seq(Json.True(1), Json.True(2), Json.False(3)),
      4
    )
    val q1 = Query(model1)

    assert(q1.as[Seq[Boolean]] === Seq(true, true, false))

    val model2: Json[Int] =
      Json.Array(Seq(
        Json.Array(Seq(Json.Number("22", 1)), 2),
        Json.Array(Seq.empty, 3),
        Json.Array(Seq(Json.Number("44.5", 4), Json.Number("22.5", 5)), 6)
      ), 7)
    val q2 = Query(model2)
    val expected2 =
      Seq(
        Seq(BigDecimal("22")),
        Seq.empty,
        Seq(BigDecimal("44.5"), BigDecimal("22.5"))
      )
    assert(q2.as[Seq[Seq[BigDecimal]]] === expected2)
  }


  test("Map derivations work") {
    val model1: Json[Int] = Json.Object(Map(
      "a" -> Json.ObjectEntry("a", 1, Json.True(2)),
      "b" -> Json.ObjectEntry("b", 3, Json.True(4)),
      "c" -> Json.ObjectEntry("c", 5, Json.False(6))
    ), 7)
    val q1 = Query(model1)

    assert(q1.as[Map[String, Boolean]] === Map("a" -> true, "b" -> true, "c" -> false))

    val model2: Json[Int] =
      Json.Object(Map(
        "a" -> Json.ObjectEntry("a", 1, Json.Object(Map(
          "tt" -> Json.ObjectEntry("tt", 2, Json.Number("22", 3))
        ), 4)),
        "b" -> Json.ObjectEntry("b", 5, Json.Object(Map.empty, 6)),
        "c" -> Json.ObjectEntry("c", 7, Json.Object(Map(
          "ff" -> Json.ObjectEntry("ff", 8, Json.Number("44.5", 9)),
          "tt" -> Json.ObjectEntry("tt", 10, Json.Number("22.5", 11))
        ), 12))
      ), 13)
    val q2 = Query(model2)
    val expected2 =
      Map(
        "a" -> Map("tt" -> BigDecimal("22")),
        "b" -> Map.empty,
        "c" -> Map("ff" -> BigDecimal("44.5"), "tt" -> BigDecimal("22.5"))
      )
    assert(q2.as[Map[String, Map[String, BigDecimal]]] === expected2)
  }

end ConversionAndSyntaxTest


