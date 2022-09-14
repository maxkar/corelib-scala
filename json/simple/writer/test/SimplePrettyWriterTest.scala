package io.github.maxkar
package json.simple.writer

import json.writer.Output
import json.simple.Json

import scala.collection.SeqMap


/**
 * Test for the writer (both json writer and writer for the simple binding).
 */
final class AttributedPrettyWriterTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Primitives are serialized as needed") {
    given options: Output.PrettyPrintOptions = Output.PrettyPrintOptions("  ", false)

    checkPretty("null", Json.Null)
    checkPretty("true", Json.True)
    checkPretty("false", Json.False)
    checkPretty("42", Json.Number("42"))
    checkPretty("-42.35E+5", Json.Number("-42.35E+5"))
    checkPretty("\"Hello, World\"", Json.String("Hello, World"))
    checkPretty("\"Hello, \\\"World\\\"\"", Json.String("Hello, \"World\""))
    checkPretty("\"Hello, \\\\\"", Json.String("Hello, \\"))
    checkPretty("\"\\r\\r\"", Json.String("\r\r"))
    checkPretty("\"\\t\\r\\n\\b\\\\\\\"\"", Json.String("\t\r\n\b\\\""))
  }


  test("Arrays are serialized as expected (basic test)") {
    given options: Output.PrettyPrintOptions =
      Output.PrettyPrintOptions(
        "  ", false,
        emptyArrayWrap = Output.alwaysWrapEmpty
      )

    checkPretty(
      """[
      |]""".stripMargin,
      Json.Array(Seq())
    )
    checkPretty(
      """[
        |  true
        |]""".stripMargin,
      Json.Array(Seq(Json.True))
    )
    checkPretty(
      """[
         |  true,
         |  false
         |]""".stripMargin,
      Json.Array(Seq(Json.True, Json.False))
    )
    checkPretty(
      """[
        |  [
        |  ]
        |]""".stripMargin,
      Json.Array(Seq(Json.Array(Seq())))
    )
  }


  test("Objects are serialized as expected (basic test)") {
    given options: Output.PrettyPrintOptions =
      Output.PrettyPrintOptions(
        "  ", false,
        emptyObjectWrap = Output.alwaysWrapEmpty
      )


    checkPretty(
      """{
        |}""".stripMargin,
      Json.Object(Map())
    )
    checkPretty(
      """{
         |  "a": true
         |}""".stripMargin,
      Json.Object(SeqMap(
        "a" -> Json.True
      ))
    )
    checkPretty(
      """{
         |  "a": true,
         |  "b": 42,
         |  "\n": {
         |  }
         |}""".stripMargin,
      Json.Object(SeqMap(
        "a" -> Json.True,
        "b" -> Json.Number("42"),
        "\n" -> Json.Object(Map()),
      ))
    )
  }


  test("Array wrapping options - root") {
    val arr1 = Json.Array(Seq.empty)

    checkPretty("""[]""", arr1)(using Output.PrettyPrintOptions(emptyArrayWrap = Output.noWrapEmpty))
    checkPretty("[]", arr1)(using Output.PrettyPrintOptions(emptyArrayWrap = Output.wrapEmptyInsideObjects))
    checkPretty("[\n]", arr1)(using Output.PrettyPrintOptions(emptyArrayWrap = Output.alwaysWrapEmpty))
  }


  test("Array wrapping options - nested") {
    val arr1 = Json.Array(Seq.empty)
    val arr2 = Json.Array(Seq(arr1, arr1))

    checkPretty(
      """[
        |    [],
        |    []
        |]""".stripMargin,
      arr2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyArrayWrap = Output.noWrapEmpty
    ))
    checkPretty(
      """[
        |    [],
        |    []
        |]""".stripMargin,
      arr2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyArrayWrap = Output.wrapEmptyInsideObjects
    ))
    checkPretty(
      """[
        |    [
        |    ],
        |    [
        |    ]
        |]""".stripMargin,
      arr2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyArrayWrap = Output.alwaysWrapEmpty
    ))
    checkPretty(
      """[
        |    [
        |    ],
        |    [
        |    ]
        |]""".stripMargin,
      arr2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyArrayWrap = Output.WrapEmptyOptions(wrapInObjects = false, wrapInArrays = true, wrapAtTopLevel = true)
    ))
  }


  test("Object wrapping options - root") {
    val obj = Json.Object(Map.empty)

    checkPretty("{}", obj)(using Output.PrettyPrintOptions(emptyObjectWrap = Output.noWrapEmpty))
    checkPretty("{}", obj)(using Output.PrettyPrintOptions(emptyObjectWrap = Output.wrapEmptyInsideObjects))
    checkPretty("{\n}", obj)(using Output.PrettyPrintOptions(emptyObjectWrap = Output.alwaysWrapEmpty))
  }


  test("Object wrapping options - nested") {

    val obj1 = Json.Object(Map.empty)
    val obj2 = Json.Object(SeqMap(
      "a" -> obj1,
      "b" -> obj1,
    ))

    checkPretty(
      """{
        |    "a": {},
        |    "b": {}
        |}""".stripMargin,
      obj2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyObjectWrap = Output.noWrapEmpty
    ))
    checkPretty(
      """{
        |    "a": {},
        |    "b": {}
        |}""".stripMargin,
      obj2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyObjectWrap = Output.WrapEmptyOptions(wrapInObjects = false, wrapInArrays = true, wrapAtTopLevel = true)
    ))
    checkPretty(
      """{
        |    "a": {
        |    },
        |    "b": {
        |    }
        |}""".stripMargin,
      obj2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyObjectWrap = Output.wrapEmptyInsideObjects
    ))
    checkPretty(
      """{
        |    "a": {
        |    },
        |    "b": {
        |    }
        |}""".stripMargin,
      obj2
    )(using Output.PrettyPrintOptions(
      "    ",
      emptyObjectWrap = Output.alwaysWrapEmpty
    ))
  }


  test("Objects inside arrays") {
    val obj1 = Json.Object(Map.empty)
    val obj2 = Json.Object(SeqMap("a" -> obj1))

    val arr = Json.Array(Seq(obj1, obj2))

    checkPretty(
      """[
        |  {},
        |  {
        |    "a": {}
        |  }
        |]""".stripMargin,
      arr
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyObjectWrap = Output.noWrapEmpty,
      )
    )

    checkPretty(
      """[
        |  {},
        |  {
        |    "a": {
        |    }
        |  }
        |]""".stripMargin,
      arr
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyObjectWrap = Output.wrapEmptyInsideObjects,
      )
    )

    checkPretty(
      """[
        |  {
        |  },
        |  {
        |    "a": {
        |    }
        |  }
        |]""".stripMargin,
      arr
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyObjectWrap = Output.alwaysWrapEmpty,
      )
    )

    checkPretty(
      """[
        |  {
        |  },
        |  {
        |    "a": {}
        |  }
        |]""".stripMargin,
      arr
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyObjectWrap = Output.WrapEmptyOptions(
          wrapInObjects = false,
          wrapInArrays = true,
          wrapAtTopLevel = false,
        ),
      )
    )
  }


  test("Arrays inside objects.") {
    val arr1 = Json.Array(Seq.empty)
    val arr2 = Json.Array(Seq(arr1))
    val obj = Json.Object(SeqMap(
      "a" -> arr1,
      "b" -> arr2,
    ))

    checkPretty(
      """{
        |  "a": [],
        |  "b": [
        |    []
        |  ]
        |}""".stripMargin,
      obj
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyArrayWrap = Output.noWrapEmpty,
      )
    )

    checkPretty(
      """{
        |  "a": [
        |  ],
        |  "b": [
        |    []
        |  ]
        |}""".stripMargin,
      obj
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyArrayWrap = Output.wrapEmptyInsideObjects,
      )
    )

    checkPretty(
      """{
        |  "a": [
        |  ],
        |  "b": [
        |    [
        |    ]
        |  ]
        |}""".stripMargin,
      obj
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyArrayWrap = Output.alwaysWrapEmpty,
      )
    )

    checkPretty(
      """{
        |  "a": [],
        |  "b": [
        |    [
        |    ]
        |  ]
        |}""".stripMargin,
      obj
    )(
      using Output.PrettyPrintOptions(
        "  ",
        emptyArrayWrap = Output.WrapEmptyOptions(
          wrapAtTopLevel = false,
          wrapInObjects = false,
          wrapInArrays = true,
        ),
      )
    )
  }


  test("Object sorting options") {
    val arr1 = Json.Array(Seq.empty)
    val arr2 = Json.Array(Seq(arr1))

    val obj = Json.Object(SeqMap(
      "b" -> arr2,
      "a" -> arr1,
    ))


    checkPretty(
      """{
        |  "b": [
        |    []
        |  ],
        |  "a": []
        |}""".stripMargin,
      obj
    )(
      using Output.PrettyPrintOptions(
        "  ",
        sortObjectKeys = false,
        emptyArrayWrap = Output.noWrapEmpty,
      )
    )

    checkPretty(
      """{
        |  "a": [],
        |  "b": [
        |    []
        |  ]
        |}""".stripMargin,
      obj
    )(
      using Output.PrettyPrintOptions(
        "  ",
        sortObjectKeys = true,
        emptyArrayWrap = Output.noWrapEmpty,
      )
    )
  }

  /**
   * Checks that the json serialization provides expected result.
   *
   * @param expected expected output.
   * @param v json to serialize.
   */
  private def checkPretty(expected: String, v: Json)(using opts: Output.PrettyPrintOptions): Unit =
    val writer = Output.pretty(v, opts)
    val result = writer.mkString("")
    assert(expected === result)
  end checkPretty
end AttributedPrettyWriterTest
