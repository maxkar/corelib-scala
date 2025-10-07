package io.github.maxkar
package json.parser.v3

import TestIO.*
import scala.collection.mutable.ArrayBuffer

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
        assert(expected === parse(inputString, Arrays.read(_, Factory))._1)
      }
  }


  test("Array format errors") {
    val data = Seq(
      ("45", 0, "Invalid array start"),
      ("[42", 3, "Invalid array separator or end"),
      ("[42:", 3, "Invalid array separator or end"),
      ("[42?", 3, "Invalid array separator or end"),
      ("[42 ?", 4, "Invalid array separator or end"),
      ("[42 , 65", 8, "Invalid array separator or end"),
    )

    for
      (inputString, offset, message) <- data
    do
      withClue(inputString) {
        val actualExn = failParse(inputString, Arrays.read(_, Factory))
        assert(offset === actualExn.offset)
        assert(message === actualExn.message)
      }
  }
}

object ArraysTest {
  object Factory extends Arrays.Factory[Operation, JsonStream, Seq[String]] {
    override type Context = ArrayBuffer[String]

    override def consumeIgnorableWhitespaces(stream: JsonStream): Operation[Unit] =
      Whitespaces.skip(stream)

    override def start(stream: JsonStream, count: Int): Operation[Context] =
      stream.skip(1) <| { _ => new Context() }

    override def badArrayStart(stream: JsonStream): Operation[Seq[String]] =
      raise(stream, "Invalid array start")

    override def consumeValue(stream: JsonStream, context: Context): Operation[Unit] =
      Numbers.read(stream, NumbersTest.Factory) <| { s => context.append(s) }

    override def consumeValueSeparator(
          stream: JsonStream,
          context: Context,
          count: Int
        ): Operation[Unit] =
      stream.skip(count)

    override def finish(
          stream: JsonStream,
          context: Context,
          count: Int
        ): Operation[Seq[String]] =
      stream.skip(count) <| { _ => context.toSeq }

    override def missingValueSeparatorOrArrayEnd(
          stream: JsonStream,
          context: Context
        ): Operation[Seq[String]] =
      raise(stream, "Invalid array separator or end")
  }
}
