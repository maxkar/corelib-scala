package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException

/** Tests for the string reader. */
final class StringReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import Unnest.given
  import StringReaderTest.given

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
    checkError("Hello\"", new StringReaderTest.IllegalStringStart(0))
    checkError("??", new StringReaderTest.IllegalStringStart(0))
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
      checkError("\"" + base, new StringReaderTest.UnterminatedString(base.length() + 1))
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
      checkError("\"" + base + "\"", new StringReaderTest.InvalidCharacter(prefix.length() + 1))
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
      checkError("\"" + base + "\"", new StringReaderTest.InvalidEscapeCharacter(prefix.length() + 1))
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
      checkError("\"" + base + "\"", new StringReaderTest.InvalidUnicodeEscape(prefix.length() + 1))
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
      checkError("\"" + base + "\"", new StringReaderTest.InvalidUnicodeEscape(prefix.length() + 1))
    }
  }


  private def checkSimpleSuccess(expected: String, input: String): Unit =
    assert(expected === read(input))


  /** Checks that error is raised. */
  private def checkError(data: String, error: Throwable): Unit =
    withClue(data) {
      val exn = intercept[IOException] { read(data) }
      assert(error === exn)
    }


  private def read(source: String): String = {
    import Unnest.given

    val sr = new java.io.StringReader(source)
    val br = BufferedLookAhead(sr, 100)
    val jssr = StringReader(br)

    Unnest.run(jssr.readString())
  }
}

object StringReaderTest {
  import Unnest.given
  type IOStream = BufferedLookAhead[java.io.Reader]


  private given unnestError: BufferedLookAhead.IOErrors[Unnest, java.io.Reader] =
    BufferedLookAhead.IOErrors.raise { [T] => (ctx, msg) => throw new IOException(msg) }


  private given stringErrors: StringReader.Errors[Unnest, IOStream] with {
    override def illegalStringStart[T](stream: IOStream): Unnest[T] =
      offset(stream) <||| { offset => throw new IllegalStringStart(offset) }

    override def invalidCharacter[T](stream: IOStream): Unnest[T] =
      offset(stream) <||| { offset => throw new InvalidCharacter(offset) }

    override def invalidEscapeCharacter[T](stream: IOStream): Unnest[T] =
      offset(stream) <||| { offset => throw new InvalidEscapeCharacter(offset) }

    override def invalidUnicodeEscape[T](stream: IOStream): Unnest[T] =
      offset(stream) <||| { offset => throw new InvalidUnicodeEscape(offset) }

    override def unterminatedString[T](stream: IOStream): Unnest[T] =
      offset(stream) <||| { offset => throw new UnterminatedString(offset) }

    private def offset(stream: IOStream): Unnest[Int] =
      stream.getLocation() <| (_.offset)
  }


  /** Reader for java instances. */
  private given javaReaderReader[M[_]: Monad, T <: java.io.Reader]: Reader[M, T] with {
    override def read(source: T, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      Monad.pure(source.read(target, targetStart, targetEnd - targetStart))
   }

  private final case class IllegalStringStart(offset: Int) extends IOException
  private final case class InvalidCharacter(offset: Int) extends IOException
  private final case class InvalidEscapeCharacter(offset: Int) extends IOException
  private final case class InvalidUnicodeEscape(offset: Int) extends IOException
  private final case class UnterminatedString(offset: Int) extends IOException
}
