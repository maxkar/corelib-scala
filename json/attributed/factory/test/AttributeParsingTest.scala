package io.github.maxkar
package json.attr.factory

import json.attr.Json

import json.parser.JsonParser

import json.parser.chunky.Module
import json.parser.chunky.SourceLocation


/** Tests for attributed parsing that keeps location. */
class AttributeParsingTest extends org.scalatest.funsuite.AnyFunSuite:
  /** Parsing module & operations. */
  private val module = Module[String](unexpectedEof = loc => s"${loc}: Unexpected EOF")
  import module.given

  /** Attributes of the resulting json. */
  type Attrs = (SourceLocation, SourceLocation)


  /** Factory of the json values. */
  private val factory =
    new Factory[module.Parser, SourceLocation, Attrs](
      location = module.location,
      locationToString = l => s"${l.line}:${l.column} (offset=${l.offset})",
      fail = msg => module.abort(msg),
      makeAttrs = (start, end) => (start, end)
    )

  /** Parser to use in tests. */
  private val parser = new JsonParser(parserInput, factory)


  test("Some basic literals work") {
    assert(runParser("true") === Right(Json.True(lineAttr(0, 1, 1, 4))))
    assert(runParser("false") === Right(Json.False(lineAttr(0, 1, 1, 5))))
    assert(runParser("null") === Right(Json.Null(lineAttr(0, 1, 1, 4))))
  }


  test("Number parsing works") {
    assert(runParser("1") === Right(Json.Number("1", lineAttr(0, 1, 1, 1))))
    assert(runParser("1.25") === Right(Json.Number("1.25", lineAttr(0, 1, 1, 4))))
    assert(runParser("1.25E+5") === Right(Json.Number("1.25E+5", lineAttr(0, 1, 1, 7))))
  }


  test("String parsing works") {
    assert(runParser("\"a\"") === Right(Json.String("a", lineAttr(0, 1, 1, 3))))
    assert(runParser("\"abc\"") === Right(Json.String("abc", lineAttr(0, 1, 1, 5))))
    assert(runParser("\"a\\u0020c\"") === Right(Json.String("a c", lineAttr(0, 1, 1, 10))))
  }


  test("Array parsing works") {
    assert(runParser("[]") === Right(Json.Array(Seq.empty, lineAttr(0, 1, 1, 2))))
    assert(runParser("[true]") === Right(
      Json.Array(
        Seq(
          Json.True(lineAttr(1, 1, 2, 4))
        ),
        lineAttr(0, 1, 1, 6))
      )
    )

    assert(runParser("[true,\n false\n]") === Right(
      Json.Array(
        Seq(
          Json.True(lineAttr(1, 1, 2, 4)),
          Json.False(lineAttr(8, 2, 2, 5)),
        ),
        multilineAttr(0, 1, 1, 15, 3, 2))
      )
    )
  }


  test("Object parsing works") {
    assert(runParser("{}") === Right(Json.Object(Map.empty, lineAttr(0, 1, 1, 2))))

    val object1 = Json.Object(
      Map(
        "vl" -> Json.ObjectEntry("vl", lineAttr(1, 1, 2, 4), Json.True(lineAttr(7, 1, 8, 4)))
      ),
      lineAttr(0, 1, 1, 12)
    )

    val object2 = Json.Object(
      Map(
        "vl" -> Json.ObjectEntry("vl", lineAttr(4, 2, 3, 4), Json.True(lineAttr(10, 2, 9, 4))),
        "vv" -> Json.ObjectEntry("vv", lineAttr(18, 3, 3, 4), Json.False(lineAttr(24, 3, 9, 5))),
      ),
      multilineAttr(0, 1, 1, 31, 4, 2)
    )

    assert(runParser("""{"vl": true}""") === Right(object1))
    assert(runParser("{\n  \"vl\": true,\n  \"vv\": false\n}") === Right(object2))
  }


  /** Creates a "single-line" attribute. */
  private def lineAttr(offset: Int, row: Int, column: Int, width: Int): Attrs =
    val start = SourceLocation(offset, row, column)
    val end = SourceLocation(offset + width, row, column + width)
    (start, end)


  /** Creates a multi-line attribute. */
  private def multilineAttr(
        startOffset: Int, startRow: Int, startCol: Int,
        endOffset: Int, endRow: Int, endCol: Int): Attrs =
    val start = SourceLocation(startOffset, startRow, startCol)
    val end = SourceLocation(endOffset, endRow, endCol)
    (start, end)


  /** Runs the parser on the given input with with the given chunk size. */
  private def runParser(input: String): Either[String, Json[Attrs]] =
    val consumer = module.start(parser.parseValue)

    var pos = 0
    while pos < input.length do
      val nextChunkSize = input.length - pos
      val consumed = consumer.process(input.subSequence(pos, pos + nextChunkSize))
      /* Consumer indicated that it does not want any more input. */
      if consumed < nextChunkSize then
        return consumer.end()

      pos += consumed
    end while

    consumer.end()
  end runParser
end AttributeParsingTest
