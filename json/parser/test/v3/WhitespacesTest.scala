package io.github.maxkar
package json.parser.v3


final class WhitespacesTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  test("Whitespaces are read skipped as expected") {
    checkRead("  Hello, world", 2)
    checkRead("   \t\r\n  Wr", 8)
    checkRead("   Test", 3)
    checkRead("    ", 4)
    checkRead("X X", 0)
    checkRead("", 0)
  }


  private def checkRead(input: String, expectedOffset: Int): Unit =
    withClue(input) {
      assert(expectedOffset == TestIO.parse(input, Whitespaces.skip)._2)
    }
}

