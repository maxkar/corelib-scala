package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException
import scala.StringContext.InvalidEscapeException


final class ObjectReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given
  import ObjectReaderTest.given
  import ObjectReaderTest.*


  test("Happy path scenarios") {
    val data = Seq(
      """{}""" -> Map(),
      """{"a":43}""" -> Map("a" -> "43"),
      """{"a":43,"b":48,"c":54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{ "a":43,"b":48,"c":54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{"a":43,"b":48,"c":54 }""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{"a": 43, "b": 48, "c": 54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{"a" :43 ,"b" :48 ,"c" :54}""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
      """{ "a" : 43 , "b" : 48 , "c" : 54 }""" -> Map("a" -> "43", "b" -> "48", "c" -> "54"),
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


  test("Object format errors") {
    val data = Seq(
      ("45", ObjectReaderTest.InvalidObjectStart(0)),
      ("""{"a" -> 45}""", ObjectReaderTest.InvalidKeyValueSeparator(5)),
      ("""{"a"?45}""", ObjectReaderTest.InvalidKeyValueSeparator(4)),
      ("""{"a": 45, "b" -> 66}""", ObjectReaderTest.InvalidKeyValueSeparator(14)),
      ("""{"a":45+"b" -> 66}""", ObjectReaderTest.InvalidObjectEnd(7)),
      ("""{"a": 45 +"b" -> 66}""", ObjectReaderTest.InvalidObjectEnd(9)),
    )

    for
      (inputString, exn) <- data
    do
      withClue(inputString) {
        val actualExn = intercept[IOException] { read(inputString) }
        assert(exn === actualExn)
      }
  }


  private def read(source: String): Map[String, String] = {
    val or = ObjectReader(stringInput(source))

    Unnest.run(or.readMap(e => StringReader(e).readString(), e => NumberReader(e).readString()))
  }
}



object ObjectReaderTest {
  import TestIO.*
  import TestIO.given


  private given objectErrors: ObjectReader.Errors[Unnest, IOStream] with {
    override def invalidObjectStart[T](stream: IOStream): Unnest[T] =
      offset(stream) <| { offset => throw new InvalidObjectStart(offset) }

    override def invalidObjectEnd[T](stream: IOStream): Unnest[T] =
      offset(stream) <| { offset => throw new InvalidObjectEnd(offset) }

    override def invalidKeyValueSeparator[T](stream: IOStream): Unnest[T] =
      offset(stream) <| { offset => throw new InvalidKeyValueSeparator(offset) }

    private def offset(stream: IOStream): Unnest[Int] =
      stream.getLocation() <| (_.offset)
  }


  private given numberErrors: NumberReader.Errors[Unnest, IOStream] with {
    override def leadingIntegerZero[T](stream: IOStream): Unnest[T] = fail()
    override def missingIntegerDigits[T](stream: IOStream): Unnest[T] = fail()
    override def missingDecimalDigits[T](stream: IOStream): Unnest[T] = fail()
    override def missingExponentDigits[T](stream: IOStream): Unnest[T] = fail()

    private def fail(): Nothing =
      throw new IOException("Unexpected number format exception")
  }


  private given stringErrors: StringReader.Errors[Unnest, IOStream] with {
    override def illegalStringStart[T](stream: IOStream): Unnest[T] = fail()
    override def invalidCharacter[T](stream: IOStream): Unnest[T] = fail()
    override def invalidEscapeCharacter[T](stream: IOStream): Unnest[T] = fail()
    override def invalidUnicodeEscape[T](stream: IOStream): Unnest[T] = fail()
    override def unterminatedString[T](stream: IOStream): Unnest[T] = fail()

    private def fail(): Nothing =
      throw new IOException("Unexpected number format exception")
  }

  private final case class InvalidObjectStart(offset: Int) extends IOException
  private final case class InvalidObjectEnd(offset: Int) extends IOException
  private final case class InvalidKeyValueSeparator(offset: Int) extends IOException
}

