package io.github.maxkar
package json.parser

/**
 * Tests for whitespace parsers.
 */
final class WhitespacesTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Count whitespaces is correct") {
    assert(0 === Whitespaces.countWhitespaces("Test"))
    assert(1 === Whitespaces.countWhitespaces(" Test"))
    assert(2 === Whitespaces.countWhitespaces(" \tTest"))
  }


  test("Next works as expected") {
    val str1 = SimpleStringStream("  Hello, world", 5)
    assert(Whitespaces.next(str1).toString() === "  ")
    assert(Whitespaces.next(str1) === null)
    assert(str1.readOffset === 2)

    val str2 = SimpleStringStream("  Hello, world", 1)
    assert(Whitespaces.next(str2).toString() === " ")
    assert(Whitespaces.next(str2).toString() === " ")
    assert(Whitespaces.next(str2) === null)
    assert(str2.readOffset === 2)

    val str3 = SimpleStringStream("  Hello, world", 2)
    assert(Whitespaces.next(str3).toString() === "  ")
    assert(Whitespaces.next(str3) === null)
    assert(str3.readOffset === 2)
  }


  test("Skipping all whitespaces works as expected") {
    SimpleStringStream.forAllLookAheadSizes[Identity]("   \t\r\n  Wr") { stream =>
      Whitespaces.skipAll(stream)
      assert(stream.readOffset === 8)
    }
  }


  test("Reading full whitespace sequence works as expected") {
    SimpleStringStream.forAllLookAheadSizes[Identity]("   \t\r\n  Wr") { stream =>
      assert(Whitespaces.readAll(stream) === "   \t\r\n  ")
      assert(stream.readOffset === 8)
    }
  }


  test("Parsing whitespaces works as expected (with various buffer sizes)") {
    val str1 = SimpleStringStream("   Test", 1)
    val (out1, hasMore1) = Whitespaces.parse(str1)
    assert(out1.toString() === " ")
    assert(hasMore1)

    val str2 = SimpleStringStream("   Test", 2)
    val (out2, hasMore2) = Whitespaces.parse(str2)
    assert(out2.toString() === "  ")
    assert(hasMore2)

    val str3 = SimpleStringStream("   Test", 3)
    val (out3, hasMore3) = Whitespaces.parse(str3)
    assert(out3.toString() === "   ")
    assert(hasMore3)

    val str4 = SimpleStringStream("   Test", 4)
    val (out4, hasMore4) = Whitespaces.parse(str4)
    assert(out4.toString() === "   ")
    assert(!hasMore4)
  }
end WhitespacesTest
