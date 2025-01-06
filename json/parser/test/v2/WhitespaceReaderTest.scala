package io.github.maxkar
package json.parser.v2

/** Tests for the whitespace reader. */
final class WhitespaceReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  test("Whitespaces are read skipped as expected") {
    checkRead("  Hello, world", "  ")
    checkRead("   \t\r\n  Wr", "   \t\r\n  ")
    checkRead("   Test", "   ")
    checkRead("    ", "    ")
    checkRead("X X", "")
    checkRead("", "")
  }


  private def checkRead(input: String, expected: String): Unit =
    assert(expected === read(input))


  private def read(source: String): String = {
    val wsr = WhitespaceReader(stringInput(source))
    doIO(wsr.readString())
  }
}

