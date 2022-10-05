package io.github.maxkar
package json.parser

import input.CharacterStream

/**
 * Tests for the series of "readXXX" methods.
 */
final class NumbersReadXXXTest extends org.scalatest.funsuite.AnyFunSuite:
  test("readNumberSign (variations)") {
    genericTest("-23", Numbers.readOptionalNumberSign, Some('-'), 1, 1)
    genericTest("423", Numbers.readOptionalNumberSign, None, 0, 1)
    genericTest("+423", Numbers.readOptionalNumberSign, None, 0, 1)

    genericTest("-23", Numbers.readNumberSignAsOptChar, '-', 1, 1)
    genericTest("423", Numbers.readNumberSignAsOptChar, 0, 0, 1)
    genericTest("+423", Numbers.readNumberSignAsOptChar, 0, 0, 1)
  }


  test("Reading start of (integer) number works") {
    genericTest("2235", Numbers.readIntegerDigitsStart, "2235", 4, 4)
    genericTest("2235", Numbers.readIntegerDigitsStart, "22", 2, 2)
    genericTest("2235.25", Numbers.readIntegerDigitsStart, "2235", 4, 6)
    genericTest("0", Numbers.readIntegerDigitsStart, "0", 1, 4)
    genericTest("0.25", Numbers.readIntegerDigitsStart, "0", 1, 4)

    genericFailure(
      "text",
      Numbers.readIntegerDigitsStart,
      NumberErrors.MissingIntegerDigits(),
      0
    )
    genericFailure(
      ".25",
      Numbers.readIntegerDigitsStart,
      NumberErrors.MissingIntegerDigits(),
      0
    )
    genericFailure(
      "000",
      Numbers.readIntegerDigitsStart,
      NumberErrors.LeadingIntegerZero(),
      0
    )
  }


  test("readDecimalSeparator (variations)") {
    genericTest(".23", Numbers.readOptionalDecimalSeparator, Some('.'), 1, 1)
    genericTest("E23", Numbers.readOptionalDecimalSeparator, None, 0, 1)

    genericTest(".45", Numbers.readDecimalSeparatorAsBoolean, true, 1, 1)
    genericTest("E23", Numbers.readDecimalSeparatorAsBoolean, false, 0, 1)

    genericTest(".45", Numbers.readDecimalSeparatorAsOptChar, '.', 1, 1)
    genericTest("E23", Numbers.readDecimalSeparatorAsOptChar, 0, 0, 1)
  }


  test("Reading start of decimal digits works") {
    genericTest("2235", Numbers.readDecimalDigitsStart, "2235", 4, 4)
    genericTest("2235", Numbers.readDecimalDigitsStart, "22", 2, 2)
    genericTest("2235.25", Numbers.readDecimalDigitsStart, "2235", 4, 6)
    genericTest("0", Numbers.readDecimalDigitsStart, "0", 1, 4)
    genericTest("0E25", Numbers.readDecimalDigitsStart, "0", 1, 4)

    genericFailure(
      "text",
      Numbers.readDecimalDigitsStart,
      NumberErrors.MissingDecimalDigits(),
      0
    )
    genericFailure(
      "E25",
      Numbers.readDecimalDigitsStart,
      NumberErrors.MissingDecimalDigits(),
      0
    )
  }



  test("readExponentIndicator (variations)") {
    genericTest("E23", Numbers.readOptionalExponentIndicator, Some('E'), 1, 1)
    genericTest("e23", Numbers.readOptionalExponentIndicator, Some('e'), 1, 1)
    genericTest(",23", Numbers.readOptionalExponentIndicator, None, 0, 1)

    genericTest("E45", Numbers.readExponentIndicatorAsBoolean, true, 1, 1)
    genericTest(",23", Numbers.readExponentIndicatorAsBoolean, false, 0, 1)

    genericTest("E45", Numbers.readExponentIndicatorAsOptChar, 'E', 1, 1)
    genericTest("e45", Numbers.readExponentIndicatorAsOptChar, 'e', 1, 1)
    genericTest(",23", Numbers.readExponentIndicatorAsOptChar, 0, 0, 1)
    genericTest("E423", Numbers.readExponentIndicatorAsOptChar, 'E', 1, 4)
    genericTest("e423", Numbers.readExponentIndicatorAsOptChar, 'e', 1, 4)
  }


  test("readExponentSign (variations)") {
    genericTest("-23", Numbers.readOptionalExponentSign, Some('-'), 1, 1)
    genericTest("423", Numbers.readOptionalExponentSign, None, 0, 1)
    genericTest("+423", Numbers.readOptionalExponentSign, Some('+'), 1, 1)

    genericTest("-23", Numbers.readExponentSignAsOptChar, '-', 1, 1)
    genericTest("423", Numbers.readExponentSignAsOptChar, 0, 0, 1)
    genericTest("+423", Numbers.readExponentSignAsOptChar, '+', 1, 1)
  }


  test("Reading start of exponent digits works") {
    genericTest("2235", Numbers.readExponentDigitsStart, "2235", 4, 4)
    genericTest("2235", Numbers.readExponentDigitsStart, "22", 2, 2)
    genericTest("2235,25", Numbers.readExponentDigitsStart, "2235", 4, 6)
    genericTest("0", Numbers.readExponentDigitsStart, "0", 1, 4)
    genericTest("0,25", Numbers.readExponentDigitsStart, "0", 1, 4)

    genericFailure(
      "text",
      Numbers.readExponentDigitsStart,
      NumberErrors.MissingExponentDigits(),
      0
    )
    genericFailure(
      ",25",
      Numbers.readExponentDigitsStart,
      NumberErrors.MissingExponentDigits(),
      0
    )
  }


  test("Reading (non-leading) digits works") {
    genericTest("2235", Numbers.readNextDigits, "2235", 4, 4)
    genericTest("2235", Numbers.readNextDigits, "22", 2, 2)
    genericTest("2235,25", Numbers.readNextDigits, "2235", 4, 6)
    genericTest("0", Numbers.readNextDigits, "0", 1, 4)
    genericTest("0,25", Numbers.readNextDigits, "0", 1, 4)
  }

  /**
   * Generic test with various input lengths.
   * @param text text to parse.
   * @param fn function used to parse the text.
   * @param expected expected value of parsing.
   * @param expectedOffset expected number of consumed characters.
   * @param bufferSize read buffer size.
   */
  private def genericTest[T](
        text: String,
        fn: CharacterStream[Identity] => T,
        expected: T,
        expectedOffset: Int,
        bufferSize: Int,
      ): Unit =
    val stream = input.SimpleStringStream[Identity](text, bufferSize)
    val res = fn(stream)
    assert(expected === res)
    assert(stream.readOffset === expectedOffset)
  end genericTest



  /** Test for generic paring failure. */
  private def genericFailure(
          text: String,
          fn: CharacterStream[Identity] => Any,
          expected: Throwable,
          expectedOffset: Int,
        ): Unit =
    input.SimpleStringStream.forAllLookAheadSizes[Identity](text) { stream =>
      try
        fn(stream)
        fail(s"${expected} exception expected")
      catch
        case e if e == expected => ()
        case other => throw other
      end try
    }
  end genericFailure
end NumbersReadXXXTest
