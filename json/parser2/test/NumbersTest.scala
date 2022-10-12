package io.github.maxkar
package json.parser

/**
 * Tests for the number parsers.
 */
final class NumbersTest extends org.scalatest.funsuite.AnyFunSuite:
  import NumbersTest.given

  /** Valid numbers - should be parsed fully. */
  val validNumbers: Seq[String] =
    val nonEmptyExps =
      for
        indicator <- Seq("E", "e")
        sign <- Seq("", "+", "-")
        digits <- Seq("23", "1", "885")
      yield s"${indicator}${sign}${digits}"

    val exps = "" +: nonEmptyExps

    for
      sign <- Seq("", "-")
      lead <- Seq("236", "0", "5")
      frac <- Seq("", ".435", ".8", ".0001")
      exp <- exps
    yield s"${sign}${lead}${frac}${exp}"
  end validNumbers


  /** Numbers with missing integer digits. */
  val missingIntegerDigits: Seq[(String, Int)] =
    Seq(
      ("-", 1),
      ("-.", 1),
      ("-E", 1),
      (".25", 0),
      ("E25", 0),
      (",", 0),
      (" ", 0),
      ("", 0),
    )


  /**
   * Numbers with leading 0 before int part. Error reporting is in
   * the "scanner" positions. Readers should expect this to be one
   * position to the left.
   */
  val leadingInt0: Seq[(String, Int)] =
    Seq(
      ("00.2235", 0),
      ("-00.225", 1),
    )


  /** Numbers with missing decimal digits. */
  val missingDecimalDigits: Seq[(String, Int)] =
    Seq(
      ("23.", 3),
      ("23.E8", 3),
      ("23.,", 3),
    )


  /** Numbers with missing exponent digits. */
  val missingExponentDigits: Seq[(String, Int)] =
    Seq(
      ("23.235E", 7),
      ("23.235Exe", 7),
      ("23.235E+", 8),
      ("23.235E-", 8),
      ("23.235E+some", 8),
      ("23.235E-some", 8),
      ("23.235E-,", 8),
      ("23.235E+", 8),
      ("23.235E-", 8),
    )


  test("Happy path tests") {
    for
      num <- validNumbers
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
    testFailure(missingIntegerDigits, new NumberErrors.MissingIntegerDigits())
  }


  test("Leading 0") {
    testFailure(leadingInt0, new NumberErrors.LeadingIntegerZero())
  }


  test("Missing decimal digits") {
    testFailure(missingDecimalDigits, new NumberErrors.MissingDecimalDigits())
  }


  test("Missing exponent digits") {
    testFailure(missingExponentDigits, new NumberErrors.MissingExponentDigits())
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
end NumbersTest


object NumbersTest:
  /** Implementation of number errors. */
  given NumberErrors: Numbers.Errors[Identity, Any] with
    /** Encoding for missing integer digits. */
    case class MissingIntegerDigits() extends Exception

    /** Encoding for missing decimal digits. */
    case class MissingDecimalDigits() extends Exception

    /** Encoding for missing exponent digits. */
    case class MissingExponentDigits() extends Exception

    /** Encoding for integer with leading 0. */
    case class LeadingIntegerZero() extends Exception

    override def missingIntegerDigits[T](stream: Any): T =
      throw new MissingIntegerDigits()

    override def leadingIntegerZero[T](stream: Any): T =
      throw new LeadingIntegerZero()

    override def missingDecimalDigits[T](stream: Any): T =
      throw new MissingDecimalDigits()

    override def missingExponentDigits[T](stream: Any): T =
      throw new MissingExponentDigits()
  end NumberErrors
end NumbersTest
