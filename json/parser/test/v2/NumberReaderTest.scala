package io.github.maxkar
package json.parser.v2

final class NumberReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  private given numberErrors: NumberReader.Errors[Operation, IOStream] =
    NumberReader.Errors.raise(raise)


  /** Valid numbers - should be parsed fully. */
  val validNumbers: Seq[String] = {
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
  }


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
    do
      withClue(inputString) {
        checkSimpleSuccess(num, inputString)
      }
  }

  test("Missing integer digits") {
    testFailure(missingIntegerDigits, "Missing integer digits")
  }


  test("Leading 0") {
    testFailure(leadingInt0, "Leading zero is not allowed")
  }


  test("Missing decimal digits") {
    testFailure(missingDecimalDigits, "Missing decimal digits")
  }


  test("Missing exponent digits") {
    testFailure(missingExponentDigits, "Missing exponent digits")
  }


  private def checkSimpleSuccess(expected: String, input: String): Unit =
    assert(expected === read(input))


  /** Checks that error is raised. */
  private def testFailure(inputs: Seq[(String, Int)], error: String): Unit = {
    for {
      (data, offset) <- inputs
    } {
      withClue(data) {
        val exn = parseWithError { read(data) }
        assert(ParseException(offset, error) === exn)
      }
    }
  }

  private def read(source: String): String = {
    val nr = NumberReader(stringInput(source))
    doIO(nr.readString())
  }
}
