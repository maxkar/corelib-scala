package io.github.maxkar
package http.headers

/** Test for the header value writer. */
final class ValueWriterTest extends org.scalatest.funsuite.AnyFunSuite {
  test("String writing") {
    check("\"hello\""){ ValueWriter.writeString("hello", _) }
    check("\"\""){ ValueWriter.writeString("", _) }
    check("\"Hello, \\\\, Test\""){ ValueWriter.writeString("Hello, \\, Test", _) }
    check("\"Hello, \\\", Test\""){ ValueWriter.writeString("Hello, \", Test", _) }
  }


  test("String or token writing") {
    check("hello"){ ValueWriter.writeStringOrToken("hello", _) }
    check("\"\""){ ValueWriter.writeStringOrToken("", _) }
    check("\"Hello, \\\\, Test\""){ ValueWriter.writeStringOrToken("Hello, \\, Test", _) }
    check("\"Hello, \\\", Test\""){ ValueWriter.writeStringOrToken("Hello, \", Test", _) }
    check("\";\""){ ValueWriter.writeStringOrToken(";", _) }
    check("\"=\""){ ValueWriter.writeStringOrToken("=", _) }
  }


  test("Parameter writing") {
    check("") { ValueWriter.writeParameters(Seq.empty, _) }
    check(";a=b") { ValueWriter.writeParameters(Seq("a" -> "b"), _) }
    check(";a=b;c=d") { ValueWriter.writeParameters(Seq("a" -> "b", "c" -> "d"), _) }
    check(";g=\"\\\"\"") { ValueWriter.writeParameters(Seq("g" -> "\""), _) }
    check(";f=json;v=1;q=0.555") { ValueWriter.writeParameters(Seq("f" -> "json", "v" -> "1", "q" -> "0.555"), _) }
  }

  /** Checks generation of the given string. */
  def check[T](expected: T)(create: StringBuilder => Unit): Unit =
    withClue(expected) {
      val buf = new StringBuilder()
      create(buf)
      assert(expected === buf.toString())
    }
}
