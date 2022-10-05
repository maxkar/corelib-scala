package io.github.maxkar
package json.parser

/**
 * Tests for the number scanner API.
 */
final class NumberScannerTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Happy path tests") {
    for
      num <- NumberSamples.validNumbers
      trailer <- Seq("", ",", " ", ", ", " ,")
      input = num + trailer
      chunkSize <- 1 until input.length()
    do
      withClue(s"${input} (by ${chunkSize})") {
        val (res, scannedLength) = scan(input, chunkSize)
        assert(res === Numbers.Scanner.Result.Success)
        assert(scannedLength === num.length())
      }
    end for
  }


  test("Missing integer digits") {
    testFailure(NumberSamples.missingIntegerDigits, Numbers.Scanner.Result.MissingIntegerDigits)
  }


  test("Leading 0") {
    testFailure(NumberSamples.leadingInt0, Numbers.Scanner.Result.DigitsAfterLeading0InIntegerPart)
  }

  test("Missing decimal digits") {
    testFailure(NumberSamples.missingDecimalDigits, Numbers.Scanner.Result.MissingDecimalDigits)
  }


  test("Missing exponent digits") {
    testFailure(NumberSamples.missingExponentDigits, Numbers.Scanner.Result.MissingExponentDigits)
  }


  /** Runs a generic failure test. */
  private def testFailure(inputs: Seq[(String, Int)], expected: Numbers.Scanner.Result): Unit =
    for
      (num, failurePosition) <- inputs
      trailer <- Seq("", ",", " ", ", ", " ,")
      input = num + trailer
      chunkSize <- 1 until input.length()
    do
      withClue(s"${input} (by ${chunkSize})") {
        val (res, scannedLength) = scan(input, chunkSize)
        assert(res === expected)
        assert(scannedLength === failurePosition)
      }
    end for
  end testFailure



  /** Runs scanner over the input. Returns scanner result and end position. */
  private def scan(input: String, chunkSize: Int): (Numbers.Scanner.Result, Int) =
    val scanner = Numbers.scanner()
    var ptr = 0
    while scanner.acceptsInput() && ptr < input.length() do
      val ret = scanner.accept(input.subSequence(ptr, Math.min(input.length(), ptr + chunkSize)))
      assert(ret > 0 || !scanner.acceptsInput())
      ptr += ret
    end while

    (scanner.end(), ptr)
  end scan
end NumberScannerTest
