package io.github.maxkar
package json.parser.v2

/** Tests for the string reader. */
final class StringReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  private given stringErrors: StringReader.Errors[Operation, IOStream] =
    StringReader.Errors.raise(raise)


  test("Smoke tests") {
    checkSimpleSuccess("Hello, world", "\"Hello, world\"")
    checkSimpleSuccess("Hello, \r\n", "\"Hello, \\r\\n\"")
    checkSimpleSuccess("Hello, \r\n\u0858", "\"Hello, \\r\\n\\u0858\"")
  }


  test("Check some simple valid sequences") {
    val inputs =
      Seq(
        "",
        "ABC",
        "Hello, world",
        "This is quite a large String?",
        "This is the \u0485"
      )

    for
      input <- inputs
      prefix <- Seq.tabulate(8) { c => "X" * c }
      suffix <- Seq.tabulate(8) { c => "Y" * c }
    do {
      val base = prefix + input + suffix
      checkSimpleSuccess(base, "\"" + base + "\"")
    }
  }


  test("Check escape sequences are parsed correctly") {
    val inputs =
      Seq(
        "\\r" -> "\r",
        "\\n" -> "\n",
        "\\u0458" -> "\u0458",
        "\\\\" -> "\\",
      )

    for
      (input, expected) <- inputs
      prefix <- Seq.tabulate(8) { c => "X" * c }
      suffix <- Seq.tabulate(8) { c => "Y" * c }
    do {
      val base = prefix + input + suffix
      checkSimpleSuccess(expected, "\"" + input + "\"")
    }
  }


  test("Check start errors") {
    checkError("Hello\"", ParseException(0, "Invalid string start"))
    checkError("??", ParseException(0, "Invalid string start"))
  }


  test("Unterminated strings raise an error properly") {
    val inputs =
      Seq(
        "",
        "ABC",
        "Hello, world",
        "This is quite a large String?",
        "This is the \u0485"
      )

    for
      input <- inputs
      prefix <- Seq.tabulate(8) { c => "X" * c }
      suffix <- Seq.tabulate(8) { c => "Y" * c }
    do {
      val base = prefix + input + suffix
      checkError("\"" + base, ParseException(base.length() + 1, "Invalid string end"))
    }
  }


  test("Bad characters raise an error") {
    val inputs =
      Seq(
        "\r",
        "\n",
        "\u0000",
        "\u001F",
      )

    for
      input <- inputs
      prefix <- Seq.tabulate(8) { c => "X" * c }
      suffix <- Seq.tabulate(8) { c => "Y" * c }
    do {
      val base = prefix + input + suffix
      /* Error position is prefix and opening quote. */
      checkError("\"" + base + "\"", ParseException(prefix.length() + 1, "Illegal character"))
    }
  }


  test("Illegale escapes raise an error") {
    val inputs =
      Seq(
        "\\c",
        "\\!",
        "\\?",
        "\\_",
      )

    for
      input <- inputs
      prefix <- Seq.tabulate(8) { c => "X" * c }
      suffix <- Seq.tabulate(8) { c => "Y" * c }
    do {
      val base = prefix + input + suffix
      /* Error position is prefix and opening quote. */
      checkError("\"" + base + "\"", ParseException(prefix.length() + 1, "Invalid escape character"))
    }
  }


  test("Illegale unicode chars (sufficient length) raise an error") {
    val inputs =
      Seq(
        "\\uuuuu",
        "\\uGH00",
        "\\u00GH",
        "\\u    ",
        "\\u____",
      )

    for
      input <- inputs
      prefix <- Seq.tabulate(8) { c => "X" * c }
      suffix <- Seq.tabulate(8) { c => "Y" * c }
    do {
      val base = prefix + input + suffix
      /* Error position is prefix and opening quote. */
      checkError("\"" + base + "\"", ParseException(prefix.length() + 1, "Invalid unicode escape"))
    }
  }


  test("Illegal unicode chars (insufficient length) raise an error") {
    val inputs =
      Seq(
        "\\uu",
        "\\uGH",
        "\\u00",
        "\\u   ",
        "\\u___",
        "\\u\"",
      )

    for
      input <- inputs
      prefix <- Seq.tabulate(8) { c => "X" * c }
    do {
      val base = prefix + input
      /* Error position is prefix and opening quote. */
      checkError("\"" + base + "\"", ParseException(prefix.length() + 1, "Invalid unicode escape"))
    }
  }


  private def checkSimpleSuccess(expected: String, input: String): Unit =
    assert(expected === read(input))


  /** Checks that error is raised. */
  private def checkError(data: String, error: Throwable): Unit =
    withClue(data) {
      val exn = parseWithError { read(data) }
      assert(error === exn)
    }


  private def read(source: String): String = {
    val jssr = StringReader(stringInput(source))
    doIO(jssr.readString())
  }
}
