package io.github.maxkar
package json.simple.query

import json.query.Query
import json.simple.Json

import json.simple.query.given
import defaultConversions.given
import ConvertibleBy.Identity.given


/**
 * Tests for simple json conversion and navigation.
 */
final class ConversionAndSyntaxTest extends org.scalatest.funsuite.AnyFunSuite:

  test("Basic query and navigation on json works") {
    val model =
      Json.Object(Map(
        "a" -> Json.True,
        "b" -> Json.Number("23")
      ))

    val q = Query(model)
    assert(q.b.as[Int] === 23)
    assert(q.a.as[Boolean] === true)
    assert(q.c.as[Option[String]] === None)

    val qm = q.as[Map[String, Query[Json]]]
    assert(qm("a").as[Boolean] === true)
  }


  test("Array derivations work") {
    val model1 = Json.Array(Seq(Json.True, Json.True, Json.False))
    val q1 = Query(model1)

    assert(q1.as[Seq[Boolean]] === Seq(true, true, false))

    val model2 =
      Json.Array(Seq(
        Json.Array(Seq(Json.Number("22"))),
        Json.Array(Seq.empty),
        Json.Array(Seq(Json.Number("44.5"), Json.Number("22.5")))
      ))
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
    val model1 = Json.Object(Map(
      "a" -> Json.True, "b" -> Json.True, "c" -> Json.False
    ))
    val q1 = Query(model1)

    assert(q1.as[Map[String, Boolean]] === Map("a" -> true, "b" -> true, "c" -> false))

    val model2 =
      Json.Object(Map(
        "a" -> Json.Object(Map("tt" -> Json.Number("22"))),
        "b" -> Json.Object(Map.empty),
        "c" -> Json.Object(Map("ff" -> Json.Number("44.5"), "tt" -> Json.Number("22.5")))
      ))
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
