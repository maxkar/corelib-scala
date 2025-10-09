package io.github.maxkar
package json.parser.v3

import TestIO.*
import TestIO.given

final class ArraysTest extends org.scalatest.funsuite.AnyFunSuite {
  import ArraysTest.*

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
        assert(expected === parse(inputString, Arrays.read(_, factory))._1)
      }
  }


  test("Array format errors") {
    val data = Seq(
      ("45", 0, "Invalid array start"),
      ("[42", 3, "Invalid value separator or array end"),
      ("[42:", 3, "Invalid value separator or array end"),
      ("[42?", 3, "Invalid value separator or array end"),
      ("[42 ?", 4, "Invalid value separator or array end"),
      ("[42 , 65", 8, "Invalid value separator or array end"),
    )

    for
      (inputString, offset, message) <- data
    do
      withClue(inputString) {
        val actualExn = failParse(inputString, Arrays.read(_, factory))
        assert(offset === actualExn.offset)
        assert(message === actualExn.message)
      }
  }
}

object ArraysTest {
  val factory = new Arrays.Factory.AsSequence[Operation, JsonStream, String](
    Numbers.read(_, NumbersTest.factory)
  )
}
