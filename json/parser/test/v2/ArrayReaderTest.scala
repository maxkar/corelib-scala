package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException
import scala.StringContext.InvalidEscapeException

final class ArrayReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import Unnest.given
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
    import Unnest.given

    val sr = new java.io.StringReader(source): java.io.Reader
    val br = BufferedLookAhead(sr, 100)
    val nr = ArrayReader(br)

    Unnest.run(nr.readSequence(e => NumberReader(e).readString()))
  }
}


object ArrayReaderTest {
  import Unnest.given
  type IOStream = BufferedLookAhead[java.io.Reader]

  private given unnestError: BufferedLookAhead.IOErrors[Unnest] with {
    override def lookAheadTooBig[T](requested: Int, supported: Int): Unnest[T] =
      throw new IOException(
        s"Look ahead ${requested} is greater than the supported amount of ${supported}"
      )
  }


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


  /** Reader for java instances. */
  private given javaReaderReader[M[_]: Monad, T <: java.io.Reader]: Reader[M, T] with {
    override def read(source: T, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      Monad.pure(source.read(target, targetStart, targetEnd - targetStart))
  }

  private final case class InvalidArrayStart(offset: Int) extends IOException
  private final case class InvalidArrayEnd(offset: Int) extends IOException

}
