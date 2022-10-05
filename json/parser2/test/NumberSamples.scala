package io.github.maxkar
package json.parser

/**
 * Various sample numbers that could be reused across tests.
 */
object NumberSamples:
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
      ("00.2235", 1),
      ("-00.225", 2),
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
end NumberSamples
