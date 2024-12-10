package io.github.maxkar
package json.parser

import fun.instances.Identity
import fun.instances.Identity.given

/**
 * Tests for the string parsers.
 */
final class StringsTest extends org.scalatest.funsuite.AnyFunSuite {
  import StringsTest.given

  test("Smoke test") {
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
    checkError("Hello\"", new StringErrors.IllegalStringStart(), 0)
    checkError("??", new StringErrors.IllegalStringStart(), 0)
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
      checkError("\"" + base, new StringErrors.UnterminatedString(), base.length())
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
      checkError("\"" + base + "\"", new StringErrors.InvalidCharacter(), prefix.length() + 1)
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
      checkError("\"" + base + "\"", new StringErrors.InvalidEscapeCharacter(), prefix.length() + 1)
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
      checkError("\"" + base + "\"", new StringErrors.InvalidUnicodeEscape(), prefix.length() + 1)
    }
  }


  test("Illegale unicode chars (insufficient length) raise an error") {
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
      checkError("\"" + base + "\"", new StringErrors.InvalidUnicodeEscape(), prefix.length() + 1)
    }
  }


  /** Checks parsing success on the given string. */
  private def checkSimpleSuccess(expected: String, data: String): Unit =
    checkSimpleSuccess(expected, data, data.length())


  /** Checks parsing success on the given string. */
  private def checkSimpleSuccess(expected: String, data: String, expectedResultOffset: Int): Unit = {
    for
      trailer <- Seq("", ",", "]", "}", "  ,  ")
      text = data + trailer
      chunkSize <- 1 until text.length()
    do
      withClue(s"${text} (by ${chunkSize})") {
        val stream = SimpleStringStream(text, chunkSize)
        assert(expected === Strings.readAll(stream))
        assert(expectedResultOffset === stream.readOffset)
      }
  }


  /**
   * Checks that error is raised at the proper location. */
  private def checkError(data: String, error: Throwable, errorOffset: Int): Unit = {
    for
      chunkSize <- 1 until data.length()
    do
      withClue(s"${data} (by ${chunkSize})") {
        try
          val stream = SimpleStringStream(data, chunkSize)
          Strings.readAll(stream)
          fail("Failure expected")
        catch
          case e if e == error => ()
          case other => throw other
        end try
      }
  }
}


object StringsTest {
  /** Implementation of the string parsing error handlers. */
  given StringErrors: Strings.Errors[Identity, Any] with {
    /** Encoding of illegal string start. */
    final case class IllegalStringStart() extends Exception
    /** Encoding of invalid escape error. */
    final case class InvalidEscapeCharacter() extends Exception
    /** Encoding of invalid unicode escape error. */
    final case class InvalidUnicodeEscape() extends Exception
    /** Encoding of invalid string character error. */
    final case class InvalidCharacter() extends Exception
    /** Encoding of unterminated string error. */
    final case class UnterminatedString() extends Exception

    override def illegalStringStart[T](stream: Any): T =
      throw new IllegalStringStart()

    override def invalidEscapeCharacter[T](stream: Any): T =
      throw new InvalidEscapeCharacter()

    override def invalidUnicodeEscape[T](stream: Any): T =
      throw new InvalidUnicodeEscape()

    override def invalidCharacter[T](stream: Any): T =
      throw new InvalidCharacter()

    override def unterminatedString[T](stream: Any): T =
      throw new UnterminatedString()
  }
}
