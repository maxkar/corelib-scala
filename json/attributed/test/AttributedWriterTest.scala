package io.github.maxkar
package json.attr

import fun.instances.Identity
import fun.instances.Identity.given

import text.output.StringBuilderStream

import json.writer.Values

/**
 * Test for the writer (both json writer and writer for the attributed binding).
 */
final class AttributedWriterTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Primitives are serialized as needed") {
    checkCompact("null", Json.Null(()))
    checkCompact("true", Json.True(43))
    checkCompact("false", Json.False("This is an attribute"))
    checkCompact("42", Json.Number("42", this))
    checkCompact("-42.35E+5", Json.Number("-42.35E+5", this))
    checkCompact("\"Hello, World\"", Json.String("Hello, World", this))
    checkCompact("\"Hello, \\\"World\\\"\"", Json.String("Hello, \"World\"", this))
    checkCompact("\"Hello, \\\\\"", Json.String("Hello, \\", this))
    checkCompact("\"\\r\\r\"", Json.String("\r\r", this))
    checkCompact("\"\\t\\r\\n\\b\\\\\\\"\"", Json.String("\t\r\n\b\\\"", this))
  }


  test("Arrays are serialized as expected") {
    checkCompact("[]", Json.Array(Seq(), 45))
    checkCompact("[true]", Json.Array(Seq(Json.True("yes")), 45))
    checkCompact("[true,false]", Json.Array(Seq(Json.True("yes"),Json.False("no")), 45))
    checkCompact("[[]]", Json.Array(Seq(Json.Array(Seq(), 88)), 45))
  }


  test("Objects are serialized as expected") {
    /* The test relies on items being written in specific order. */
    import scala.collection.SeqMap

    checkCompact("{}", Json.Object(Map(), "Test"))
    checkCompact(
      "{\"a\":true}",
      Json.Object(
        SeqMap(
          "a" -> Json.ObjectEntry("a", this, Json.True("This is A"))
        ),
        "Test"
      )
    )
    checkCompact(
      "{\"a\":true,\"b\":42,\"\\n\":{}}",
      Json.Object(
        SeqMap(
          "a" -> Json.ObjectEntry("a", this, Json.True("This is A")),
          "b" -> Json.ObjectEntry("b", this, Json.Number("42", "This is B")),
          "\n" -> Json.ObjectEntry("\n", this, Json.Object(Map(), 75)),
        ),
        "Test"
      )
    )
  }


  /**
   * Checks that the json serialization provides expected result.
   *
   * @param expected expected output.
   * @param v json to serialize.
   */
  private def checkCompact[T](expected: String, v: Json[T]): Unit =
    assert(expected === v.toCompactString())
}
