package io.github.maxkar
package json.simple.writer

import json.writer.Output
import json.simple.Json


/**
 * Test for the writer (both json writer and writer for the simple binding).
 */
final class AttributedWriterTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Primitives are serialized as needed") {
    checkCompact("null", Json.Null)
    checkCompact("true", Json.True)
    checkCompact("false", Json.False)
    checkCompact("42", Json.Number("42"))
    checkCompact("-42.35E+5", Json.Number("-42.35E+5"))
    checkCompact("\"Hello, World\"", Json.String("Hello, World"))
    checkCompact("\"Hello, \\\"World\\\"\"", Json.String("Hello, \"World\""))
    checkCompact("\"Hello, \\\\\"", Json.String("Hello, \\"))
    checkCompact("\"\\r\\r\"", Json.String("\r\r"))
    checkCompact("\"\\t\\r\\n\\b\\\\\\\"\"", Json.String("\t\r\n\b\\\""))
  }


  test("Arrays are serialized as expected") {
    checkCompact("[]", Json.Array(Seq()))
    checkCompact("[true]", Json.Array(Seq(Json.True)))
    checkCompact("[true,false]", Json.Array(Seq(Json.True, Json.False)))
    checkCompact("[[]]", Json.Array(Seq(Json.Array(Seq()))))
  }


  test("Objects are serialized as expected") {
    /* The test relies on items being written in specific order. */
    import scala.collection.SeqMap

    checkCompact("{}", Json.Object(Map()))
    checkCompact(
      "{\"a\":true}",
      Json.Object(SeqMap("a" -> Json.True))
    )
    checkCompact(
      "{\"a\":true,\"b\":42,\"\\n\":{}}",
      Json.Object( SeqMap(
        "a" -> Json.True,
        "b" -> Json.Number("42"),
        "\n" -> Json.Object(Map()),
      ))
    )
  }


  test("Unicode chars are encoded as required") {
    checkCompact("\"\\u0001\"", Json.String("\u0001"))
    checkCompact("\"\\u001A\"", Json.String("\u001A"))
  }


  /**
   * Checks that the json serialization provides expected result.
   *
   * @param expected expected output.
   * @param v json to serialize.
   */
  private def checkCompact(expected: String, v: Json): Unit =
    val writer = Output.compact(v)
    val result = writer.mkString("")
    assert(expected === result)
  end checkCompact
end AttributedWriterTest
