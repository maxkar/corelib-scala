package io.github.maxkar
package json.parser.v2


final class ObjectReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  private given numberErrors: NumberReader.Errors[Operation, IOStream] =
    NumberReader.Errors.raise(raise)
  private given stringErrors: StringReader.Errors[Operation, IOStream] =
    StringReader.Errors.raise(raise)
  private given objectErrors: ObjectReader.Errors[Operation, IOStream] =
    ObjectReader.Errors.raise(raise)


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
      ("45", ParseException(0, "Invalid object start")),
      ("""{"a" -> 45}""", ParseException(5, "Invalid key-value separator")),
      ("""{"a"?45}""", ParseException(4, "Invalid key-value separator")),
      ("""{"a": 45, "b" -> 66}""", ParseException(14, "Invalid key-value separator")),
      ("""{"a":45+"b" -> 66}""", ParseException(7, "Invalid entry separator or object end")),
      ("""{"a": 45 +"b" -> 66}""", ParseException(9, "Invalid entry separator or object end")),
    )

    for
      (inputString, exn) <- data
    do
      withClue(inputString) {
        val actualExn = parseWithError { read(inputString) }
        assert(exn === actualExn)
      }
  }


  private def read(source: String): Map[String, String] = {
    val or = ObjectReader(stringInput(source))
    doIO(or.readMap(e => StringReader(e).readString(), e => NumberReader(e).readString()))
  }
}
