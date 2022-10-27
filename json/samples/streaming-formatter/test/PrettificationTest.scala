package io.github.maxkar
package json.sample.formatter.streaming

import java.io.StringReader
import java.io.StringWriter

/** Tests for JSON prettyfication. */
final class PrettyficationTest extends org.scalatest.funsuite.AnyFunSuite:


  test("Smoke test") {
    check("true")("  true  ")
    check("false")("  false  ")
    check("-12.65e4")("-12.65e4")
    check("[]")("[ ]")
    check("{}")("{ }")
    check(
      """[
        |  [],
        |  [],
        |  []
        |]""".stripMargin
    )(
      "[ [ ] , [ ] , [] ]"
    )
    check(
      """{
        |  "a": {},
        |  "b": [],
        |  "c": 5
        |}""".stripMargin
    )(
      """ { "a": {
         },
         "b": [ ], "c"
         : 5
      }"""
    )
    check("\"hello,\\n\"")("  \"hello,\\n\" ")
  }


  /** Runs the compaction and checks the result. */
  private def check(expected: String)(input: String): Unit =
    val i = new StringReader(input)
    val o = new StringWriter()
    assert(None === Formatter.prettify(i, o))
    assert(expected === o.toString())
  end check
end PrettyficationTest
