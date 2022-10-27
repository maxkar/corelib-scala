package io.github.maxkar
package json.parser

import fun.instances.Identity
import fun.instances.Identity.given

/**
 * Tests for value parser.
 */
final class ValuesTest extends org.scalatest.funsuite.AnyFunSuite:
  import ValuesTest.given
  import ValuesTest._


  test("Basic smoke tests") {
    val data = Seq(
      "true" -> java.lang.Boolean.TRUE,
      "false" -> java.lang.Boolean.FALSE,
      "645.5" -> BigDecimal("645.5"),
      "-645.5" -> BigDecimal("-645.5"),
      "[]" -> Seq.empty,
      "null" -> null,
      "{}" -> Map.empty,
      "\"Hello\"" -> "Hello",
      """[null]""" -> Seq(null),
      """["Hello", "World" ]""" -> Seq("Hello", "World"),
      """["Hello", 44 ]""" -> Seq("Hello", BigDecimal("44")),
      """{"a": 22, "b": true}""" -> Map("a" -> BigDecimal("22"), "b" -> java.lang.Boolean.TRUE),
      """{"a": [true], "b": {"c": "42", "d": []}}""" ->
        Map("a" -> Seq(java.lang.Boolean.TRUE), "b" -> Map("c" -> "42", "d" -> Seq.empty)),
    )

    for
      (inputBase, expected) <- data
      lpad <- Seq.tabulate(5) { x => " " * x }
      rpad <- Seq.tabulate(5) { x => " " * x }
      inputString = lpad + inputBase + rpad
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        val res = Values.readSimple(TestValueBuilder, stream)
        assert(res === expected)
        assert(stream.readOffset === (inputBase.length() + lpad.length()))
      }
    end for
  }

  test("Some basic errors tests") {
    import ObjectsTest.ObjectErrors
    import StringsTest.StringErrors
    import ArraysTest.ArrayErrors

    val data = Seq(
      ("""xref""", new ValueErrors.IllegalValue(), 0),
      ("""[[], xref]""", new ValueErrors.IllegalValue(), 5),
      ("""[{,}]""", new StringErrors.IllegalStringStart(), 2),
      ("""[{  ,}]""", new StringErrors.IllegalStringStart(), 4),
      ("""[{  a:b}]""", new StringErrors.IllegalStringStart(), 4),
      ("""[{  "a":b}]""", new ValueErrors.IllegalValue(), 8),
      ("""[{}""", new ArrayErrors.InvalidArraySeparator(), 3),
    )

    for
      (inputString, exn, offset) <- data
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        try
          Values.readSimple(TestValueBuilder, stream)
          fail(s"Expected ${exn} but got nothing")
        catch
          case e if e === exn => ()
          case other => throw other
        end try
        assert(stream.readOffset === offset)
      }
    end for
  }

end ValuesTest


object ValuesTest:
  /** Value errors implementation. */
  given ValueErrors: Values.Errors[Identity, Any] with
    /** Encoding for invalid value. */
    case class IllegalValue() extends Exception

    def illegalValue[T](stream: Any): T =
      throw new IllegalValue()
  end ValueErrors


  given AllValueErrors: Values.AllErrors[Identity, Any] with
    override given valueErrors: Values.Errors[Identity, Any] =
      ValueErrors
    override given literalErrors: Literals.Errors[Identity, Any] =
      LiteralsTest.LiteralErrors
    override given numberErrors: Numbers.Errors[Identity, Any] =
      NumbersTest.NumberErrors
    override given stringErrors: Strings.Errors[Identity, Any] =
      StringsTest.StringErrors
    override given arrayErrors: Arrays.Errors[Identity, Any] =
      ArraysTest.ArrayErrors
    override given objectErrors: Objects.Errors[Identity, Any] =
      ObjectsTest.ObjectErrors
  end AllValueErrors


  /** Simple model generator. */
  object TestValueBuilder extends Values.SimpleBuilder[AnyRef]:
    override def fromNull(): AnyRef = null
    override def fromBoolean(v: Boolean): AnyRef = java.lang.Boolean.valueOf(v)
    override def fromNumber(repr: String): AnyRef = BigDecimal(repr)
    override def fromString(value: String): AnyRef = value
    override def fromArray(items: Seq[AnyRef]): AnyRef = items
    override def fromObject(items: Map[String, AnyRef]): AnyRef = items
  end TestValueBuilder
end ValuesTest
