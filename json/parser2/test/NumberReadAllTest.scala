package io.github.maxkar
package json.parser

/**
 * Tests for "read all" functionality. Also covers "stateful parsing" as this parsing
 * is used by the readAll.
 */
final class NumberReadAllTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Happy path tests") {
    for
      num <- NumberSamples.validNumbers
      trailer <- Seq("", ",", " ", ", ", " ,")
      inputString = num + trailer
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new input.SimpleStringStream(inputString, chunkSize)
        assert(num === Numbers.readAll(stream))
        assert(stream.readOffset === num.length())
      }
    end for
  }

  test("Missing integer digits") {
    testFailure(NumberSamples.missingIntegerDigits, new NumberErrors.MissingIntegerDigits())
  }


  test("Leading 0") {
    testFailure(NumberSamples.leadingInt0.map((str, err) => (str, err-1)), new NumberErrors.LeadingIntegerZero())
  }


  test("Missing decimal digits") {
    testFailure(NumberSamples.missingDecimalDigits, new NumberErrors.MissingDecimalDigits())
  }


  test("Missing exponent digits") {
    testFailure(NumberSamples.missingExponentDigits, new NumberErrors.MissingExponentDigits())
  }


  /** Runs a generic failure test. */
  private def testFailure(inputs: Seq[(String, Int)], expected: Throwable): Unit =
    for
      (num, failurePosition) <- inputs
      trailer <- Seq("", ",", " ", ", ", " ,")
      inputString = num + trailer
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new input.SimpleStringStream(inputString, chunkSize)
        try
          Numbers.readAll(stream)
          fail(s"Exception ${expected} expected")
        catch
          case e if e == expected => ()
          case other => throw other
        end try
        assert(stream.readOffset === failurePosition)
      }
    end for
  end testFailure
end NumberReadAllTest
