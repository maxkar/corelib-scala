package io.github.maxkar
package json.sample.formatter.streaming

import java.io.StringReader
import java.io.StringWriter

/** Tests for JSON compaction. */
final class CompactionTest extends org.scalatest.funsuite.AnyFunSuite:


  test("Smoke test") {
    check("true")("  true  ")
    check("false")("  false  ")
    check("-12.65e4")("-12.65e4")
    check("[]")("[ ]")
    check("{}")("{ }")
    check("[[],[],[]]")("[ [ ] , [ ] , [ ] ]")
    check("""{"a":{},"b":[],"c":5}""")(
      """ {
         "a": {
         },
         "b": [
         ],
         "c": 5
      }"""
    )
    check("\"hello,\\n\"")("  \"hello,\\n\" ")
  }


  test("Stacklessness test") {
    val bignum = 100000
    val expected =
      ("{\"a\":[" * bignum) +
      "\"" + ("a" * bignum) + "\"" +
      ("]}" * bignum)

    val provided =
      ("{\n  \"a\" :  \n [  \n " * bignum) +
      "   \"" + ("a" * bignum) + "\"   " +
      ("  ]  \n  }  " * bignum)

    check(expected)(provided)
  }

  /** Runs the compaction and checks the result. */
  private def check(expected: String)(input: String): Unit =
    val i = new StringReader(input)
    val o = new StringWriter()
    assert(None === Formatter.compact(i, o))
    assert(expected === o.toString())
  end check
end CompactionTest
