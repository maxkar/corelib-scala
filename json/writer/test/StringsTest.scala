package io.github.maxkar
package json.writer

import text.output.Stream
import text.output.StringBuilderStream

import fun.instances.Identity
import fun.instances.Identity.given


/** Test for string writers.  */
final class StringsTest extends org.scalatest.funsuite.AnyFunSuite {

  /** Very simple strings - no special encoding happens. */
  private val simpleStrings = Seq(
    "hello",
    "world",
    " ",
    "!!!?",
    "\u4065"
  )

  /** Special strings with their corresponding escape sequences. */
  private val specialStrings = Seq(
    "\r" -> "\\r",
    "\n" -> "\\n",
    "\f" -> "\\f",
    "\t" -> "\\t",
    "\b" -> "\\b",
    "\"" -> "\\\"",
    "\\" -> "\\\\",
    "\u0000" -> "\\u0000",
    "\u0003" -> "\\u0003",
    "\u001F" -> "\\u001F"
  )


  /** All strings with their translations. */
  private val allStrings =
    simpleStrings.map(s => (s, s)) ++ specialStrings :+ ("" -> "")


  test("Smoke tests") {
    for
      (str, out) <- allStrings
    do
      expect(out, str)
  }


  test("Combinations(2) tests") {
    for
      (str1, out1) <- allStrings
      (str2, out2) <- allStrings
    do
      expect(out1 + out2, str1 + str2)
  }


  test("Combinations(3) tests") {
    for
      (str1, out1) <- allStrings
      (str2, out2) <- allStrings
      (str3, out3) <- allStrings
    do
      expect(out1 + out2 + out3, str1 + str2 + str3)
  }


  test("Combinations(4) tests") {
    for
      (str1, out1) <- allStrings
      (str2, out2) <- allStrings
      (str3, out3) <- allStrings
      (str4, out4) <- allStrings
    do
      expect(out1 + out2 + out3 + out4, str1 + str2 + str3 + str4)
  }

  private def expect(expected: String, input: String): Unit =
    withClue(input) {
      val stream = StringBuilderStream()
      Strings.writeString(input, stream)
      assert("\"" + expected + "\""=== stream.data)
    }
}
