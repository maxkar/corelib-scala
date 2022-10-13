package io.github.maxkar
package json.parser

/** Tests for array-related functionality. */
final class ArraysTest extends org.scalatest.funsuite.AnyFunSuite:
  import NumbersTest.given
  import ArraysTest.given


  test("Happy path scenarios") {
    val data = Seq(
      "[]" -> Seq(),
      "[43]" -> Seq("43"),
      "[43,85,87]" -> Seq("43", "85", "87"),
      "[ 43,85,87]" -> Seq("43", "85", "87"),
      "[43,85,87 ]" -> Seq("43", "85", "87"),
      "[43, 85, 87]" -> Seq("43", "85", "87"),
      "[43 ,85 ,87]" -> Seq("43", "85", "87"),
      "[ 43 , 85 , 87 ]" -> Seq("43", "85", "87"),
    )

    for
      (inputBase, expected) <- data
      rpad <- Seq.tabulate(5) { x => "X" * x }
      inputString = inputBase + rpad
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        val res = Arrays.readAll(Whitespaces.skipAll[Identity], Numbers.readAll, stream)
        assert(res === expected)
        assert(stream.readOffset === inputBase.length())
      }
    end for
  }


  test("Array format errors") {
    val data = Seq(
      ("45", new ArrayErrors.InvalidArrayStart(), 0),
      ("[42", new ArrayErrors.InvalidArraySeparator(), 3),
      ("[42:", new ArrayErrors.InvalidArraySeparator(), 3),
      ("[42?", new ArrayErrors.InvalidArraySeparator(), 3),
      ("[42 ?", new ArrayErrors.InvalidArraySeparator(), 4),
      ("[42 , 65", new ArrayErrors.InvalidArraySeparator(), 8),
    )

    for
      (inputString, exn, offset) <- data
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        try
          Arrays.readAll(Whitespaces.skipAll[Identity], Numbers.readAll, stream)
          fail(s"Expected ${exn} but got nothing")
        catch
          case e if e === exn => ()
          case other => throw other
        end try
        assert(stream.readOffset === offset)
      }
    end for
  }
end ArraysTest


object ArraysTest:
  /** Array errors implementation. */
  given ArrayErrors: Arrays.Errors[Identity, Any] with
    /** Encoding of invalid array start. */
    final case class InvalidArrayStart() extends Exception

    /** Encoding for invalid value separator. */
    final case class InvalidArraySeparator() extends Exception

    override def invalidArrayStart[T](stream: Any): T =
      throw new InvalidArrayStart()

    override def invalidArrayEnd[T](stream: Any): Identity[T] =
      throw new InvalidArraySeparator()
  end ArrayErrors
end ArraysTest
