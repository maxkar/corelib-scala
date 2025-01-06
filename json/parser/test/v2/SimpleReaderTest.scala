package io.github.maxkar
package json.parser.v2

final class SimpleReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  private given parseErrors: SimpleReader.Errors[Operation, IOStream] =
    SimpleReader.Errors.raise(raise)


  private val jsonReader =
    SimpleReader[Operation, IOStream, AnyRef](
      SimpleReaderTest.TestValueBuilder
    )


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
    do
      withClue(inputString) {
        assert(expected === read(inputString))
      }
  }


  test("Some basic errors tests") {
    val data = Seq(
      ("""xref""", 0),
      ("""[[], xref]""", 5),
      ("""[{,}]""", 2),
      ("""[{  ,}]""", 4),
      ("""[{  a:b}]""", 4),
      ("""[{  "a":b}]""", 8),
      ("""[{}""", 3),
    )

    for
      (inputString, offset) <- data
    do
      withClue(inputString) {
        val exn = parseWithError { read(inputString) }
        assert(offset === exn.offset)
      }
  }


  private def read(source: String): Object =
    doIO(jsonReader.readValue(stringInput(source)))
}


object SimpleReaderTest {
  /** Simple model generator. */
  object TestValueBuilder extends SimpleReader.Builder[AnyRef] {
    override def fromNull(): AnyRef = null
    override def fromBoolean(v: Boolean): AnyRef = java.lang.Boolean.valueOf(v)
    override def fromNumber(repr: String): AnyRef = BigDecimal(repr)
    override def fromString(value: String): AnyRef = value
    override def fromArray(items: Seq[AnyRef]): AnyRef = items
    override def fromObject(items: Map[String, AnyRef]): AnyRef = items
  }
}
