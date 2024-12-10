package io.github.maxkar
package json.parser

import fun.instances.Identity
import fun.instances.Identity.given


/** Tests for object-related functionality. */
final class ObjectsTest extends org.scalatest.funsuite.AnyFunSuite {
  import NumbersTest.given
  import StringsTest.given
  import ObjectsTest.given


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
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        val res = Objects.readAll(Whitespaces.skipAll[Identity], Strings.readAll, Numbers.readAll, stream)
        assert(res === expected)
        assert(stream.readOffset === inputBase.length())
      }
  }


  test("Object format errors") {
    val data = Seq(
      ("45", new ObjectErrors.InvalidObjectStart(), 0),
      ("""{"a" -> 45}""", new ObjectErrors.InvalidKeyValueSeparator(), 5),
      ("""{"a"?45}""", new ObjectErrors.InvalidKeyValueSeparator(), 4),
      ("""{"a": 45, "b" -> 66}""", new ObjectErrors.InvalidKeyValueSeparator(), 14),
      ("""{"a":45+"b" -> 66}""", new ObjectErrors.InvalidEntrySeparator(), 7),
      ("""{"a": 45 +"b" -> 66}""", new ObjectErrors.InvalidEntrySeparator(), 9),
    )

    for
      (inputString, exn, offset) <- data
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        try {
          Objects.readAll(Whitespaces.skipAll[Identity], Strings.readAll, Numbers.readAll, stream)
          fail(s"Expected ${exn} but got nothing")
        } catch {
          case e if e === exn => ()
          case other => throw other
        }
        assert(stream.readOffset === offset)
      }
  }
}


object ObjectsTest {
  /** Object errors implementation. */
  given ObjectErrors: Objects.Errors[Identity, Any] with {
    /** Encoding of invalid object start. */
    final case class InvalidObjectStart() extends Exception

    /** Encoding for invalid entry separator. */
    final case class InvalidEntrySeparator() extends Exception

    /** Encoding for invalid key-value separator. */
    final case class InvalidKeyValueSeparator() extends Exception

    override def invalidObjectStart[T](stream: Any): T =
      throw new InvalidObjectStart()

    override def invalidObjectEnd[T](stream: Any): Identity[T] =
      throw new InvalidEntrySeparator()

    override def invalidKeyValueSeparator[T](stream: Any): Identity[T] =
      throw new InvalidKeyValueSeparator()
  }
}
