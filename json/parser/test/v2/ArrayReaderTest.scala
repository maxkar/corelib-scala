package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException


final class ArrayReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  import ArrayReaderTest.given


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
      ("45", ArrayReaderTest.InvalidArrayStart(0)),
      ("[42", ArrayReaderTest.InvalidArrayEnd(3)),
      ("[42:", ArrayReaderTest.InvalidArrayEnd(3)),
      ("[42?", ArrayReaderTest.InvalidArrayEnd(3)),
      ("[42 ?", ArrayReaderTest.InvalidArrayEnd(4)),
      ("[42 , 65", ArrayReaderTest.InvalidArrayEnd(8)),
    )

    for
      (inputString, exn) <- data
    do
      withClue(inputString) {
        val actualExn = intercept[IOException] { read(inputString) }
        assert(exn === actualExn)
      }
  }


  private def read(source: String): Seq[String] = {
    val nr = ArrayReader(stringInput(source))
    Unnest.run(nr.readSequence(e => NumberReader(e).readString()))
  }
}


object ArrayReaderTest {
  import Unnest.given
  type IOStream = BufferedLookAhead[java.io.Reader]

  private given unnestError: BufferedLookAhead.IOErrors[Unnest, java.io.Reader] =
    BufferedLookAhead.IOErrors.raise { [T] => (ctx, msg) => throw new IOException(msg) }


  private given arrayErrors: ArrayReader.Errors[Unnest, IOStream] with {
    override def invalidArrayStart[T](stream: IOStream): Unnest[T] =
      offset(stream) <| { offset => throw new InvalidArrayStart(offset) }

    override def invalidArrayEnd[T](stream: IOStream): Unnest[T] =
      offset(stream) <| { offset => throw new InvalidArrayEnd(offset) }

    private def offset(stream: IOStream): Unnest[Int] =
      stream.getLocation() <| (_.offset)
  }


  private given numberErrors: NumberReader.Errors[Unnest, IOStream] with {
    override def leadingIntegerZero[T](stream: IOStream): Unnest[T] =
      fail()

    override def missingIntegerDigits[T](stream: IOStream): Unnest[T] =
      fail()

    override def missingDecimalDigits[T](stream: IOStream): Unnest[T] =
      fail()

    override def missingExponentDigits[T](stream: IOStream): Unnest[T] =
      fail()


    private def fail(): Nothing =
      throw new IOException("Unexpected number format exception")
  }


  private final case class InvalidArrayStart(offset: Int) extends IOException
  private final case class InvalidArrayEnd(offset: Int) extends IOException

}
