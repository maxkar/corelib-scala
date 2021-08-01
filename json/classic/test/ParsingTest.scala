package io.github.maxkar
package json.classic

/** Tests for parsing. */
class ParsingTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Negative exponent is parsed") {
    Json.parse("""{"a": 13E-5}""")
  }

  test("Positive exponent is parsed") {
    Json.parse("""{"a": 13E+5}""")
  }

  test("No-sign exponent is parsed") {
    Json.parse("""{"a": 13E5}""")
  }
end ParsingTest
