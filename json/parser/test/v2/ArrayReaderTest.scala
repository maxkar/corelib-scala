package io.github.maxkar
package json.parser.v2

final class ArrayReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  private given arrayErrors: ArrayReader.Errors[Operation, IOStream] =
    ArrayReader.Errors.raise(raise)

  private given numberErrors: NumberReader.Errors[Operation, IOStream] =
    NumberReader.Errors.raise(raise)


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
    do
      withClue(inputString) {
        assert(expected === read(inputString))
      }
  }


  test("Array format errors") {
    val data = Seq(
      ("45", ParseException(0, "Invalid array start")),
      ("[42", ParseException(3, "Invalid array separator or end")),
      ("[42:", ParseException(3, "Invalid array separator or end")),
      ("[42?", ParseException(3, "Invalid array separator or end")),
      ("[42 ?", ParseException(4, "Invalid array separator or end")),
      ("[42 , 65", ParseException(8, "Invalid array separator or end")),
    )

    for
      (inputString, exn) <- data
    do
      withClue(inputString) {
        val actualExn = parseWithError { read(inputString) }
        assert(exn === actualExn)
      }
  }


  private def read(source: String): Seq[String] = {
    val nr = ArrayReader(stringInput(source))
    doIO(nr.readSequence(e => NumberReader(e).readString()))
  }
}
