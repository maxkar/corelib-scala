package io.github.maxkar
package json.parser.v3

import TestIO.*

final class StringsTest extends org.scalatest.funsuite.AnyFunSuite {
  import StringsTest.*

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
    checkError("Hello\"", 0, "Invalid string start")
    checkError("??", 0, "Invalid string start")
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
      checkError("\"" + base, base.length() + 1, "Unterminated string")
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
      checkError("\"" + base + "\"", prefix.length() + 1, "Illegal character")
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
      checkError("\"" + base + "\"", prefix.length() + 1, "Invalid escape character")
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
      checkError("\"" + base + "\"", prefix.length() + 1, "Invalid unicode escape")
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
      checkError("\"" + base + "\"", prefix.length() + 1, "Invalid unicode escape")
    }
  }


  private def checkSimpleSuccess(expected: String, input: String): Unit =
    withClue(input) {
      val (result, offset) = parse(input, Strings.read(_, Factory))
      assert(expected === result)
      assert(input.length() === offset)
    }


  /** Checks that error is raised. */
  private def checkError(data: String, offset: Int, message: String) =
    withClue(data) {
      val exn = failParse(data, Strings.read(_, Factory))
      assert(offset === exn.offset)
      assert(message === exn.message)
    }
}

object StringsTest {
  object Factory extends Strings.Factory[Operation, JsonStream, String] {
    override type Context = StringBuilder

    override def start(stream: JsonStream, count: Int): Operation[Context] =
      stream.skip(1) <| { _ => new Context()}

    override def badStringStart(stream: JsonStream): Operation[StringBuilder] =
      raise(stream, s"Invalid string start")

    override def consumeWhile(
          stream: JsonStream,
          context: Context,
          predicate: Char => Boolean
        ): Operation[Unit] =
      stream.readWhile(context, predicate)

    override def consumeEscape(
          stream: JsonStream,
          context: Context,
          count: Int,
          char: Char
        ): Operation[Unit] = {
      context.append(char)
      stream.skip(count)
    }

    override def invalidEscapeCharacter(stream: JsonStream, context: Context): Operation[Unit] =
      raise(stream, "Invalid escape character")

    override def invalidUnicodeEscape(stream: JsonStream, context: Context): Operation[Unit] =
      raise(stream, "Invalid unicode escape")

    override def invalidCharacter(stream: JsonStream, context: Context): Operation[Unit] =
      raise(stream, "Illegal character")

    override def finish(stream: JsonStream, context: Context, count: Int): Operation[String] =
      stream.skip(count) <| { _ => context.toString() }

    override def unterminatedString(stream: JsonStream, context: Context): Operation[String] =
      raise(stream, "Unterminated string")
  }
}
