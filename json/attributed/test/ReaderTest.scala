package io.github.maxkar
package json.attr

import text.Location

import json.parser.Values.AllErrors
import json.parser.Errors
import Json.ObjectEntry
import io.github.maxkar.json.parser.v2.SimpleReader


/** Tests for attributed parsing reader. */
class ReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import json.parser.v2.TestIO.*
  import json.parser.v2.TestIO.given

  /** Attributes of the resulting json. */
  type Attrs = (Location, Location)

  /** Factory for the attributes. */
  private val attrFactory = new AttributeFactory[Operation, IOStream, Attrs] {
    override type Context = text.Location

    override def start(stream: IOStream): Operation[Context] = stream.getLocation()

    override def end(context: Context, stream: IOStream): Operation[Attrs] =
      stream.getLocation() >-> { endLoc => (context, endLoc) }
  }

  /** Attribute-specific errors. */
  given attrErrors: Reader.Errors[Operation, IOStream, Attrs] = Reader.Errors.raise(raise)
  given simpleErrors: SimpleReader.Errors[Operation, IOStream] = SimpleReader.Errors.raise(raise)

  test("Some basic literals work") {
    assert(runParser("true") === Json.True(lineAttr(0, 1, 1, 4)))
    assert(runParser("false") === Json.False(lineAttr(0, 1, 1, 5)))
    assert(runParser("null") === Json.Null(lineAttr(0, 1, 1, 4)))
  }


  test("Number parsing works") {
    assert(runParser("1") === Json.Number("1", lineAttr(0, 1, 1, 1)))
    assert(runParser("1.25") === Json.Number("1.25", lineAttr(0, 1, 1, 4)))
    assert(runParser("1.25E+5") === Json.Number("1.25E+5", lineAttr(0, 1, 1, 7)))
  }


  test("String parsing works") {
    assert(runParser("\"a\"") === Json.String("a", lineAttr(0, 1, 1, 3)))
    assert(runParser("\"abc\"") === Json.String("abc", lineAttr(0, 1, 1, 5)))
    assert(runParser("\"a\\u0020c\"") === Json.String("a c", lineAttr(0, 1, 1, 10)))
  }


  test("Array parsing works") {
    assert(runParser("[]") === Json.Array(Seq.empty, lineAttr(0, 1, 1, 2)))
    assert(runParser("[true]") ===
      Json.Array(
        Seq(
          Json.True(lineAttr(1, 1, 2, 4))
        ),
        lineAttr(0, 1, 1, 6)
      )
    )

    assert(runParser("[true,\n false\n]") ===
      Json.Array(
        Seq(
          Json.True(lineAttr(1, 1, 2, 4)),
          Json.False(lineAttr(8, 2, 2, 5)),
        ),
        multilineAttr(0, 1, 1, 15, 3, 2)
      )
    )
  }


  test("Object parsing works") {
    assert(runParser("{}") === Json.Object(Map.empty, lineAttr(0, 1, 1, 2)))

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

    assert(runParser("""{"vl": true}""") === object1)
    assert(runParser("{\n  \"vl\": true,\n  \"vv\": false\n}") === object2)
  }


  /** Creates a "single-line" attribute. */
  private def lineAttr(offset: Int, row: Int, column: Int, width: Int): Attrs = {
    val start = Location(offset, row, column)
    val end = Location(offset + width, row, column + width)
    (start, end)
  }


  /** Creates a multi-line attribute. */
  private def multilineAttr(
        startOffset: Int, startRow: Int, startCol: Int,
        endOffset: Int, endRow: Int, endCol: Int): Attrs = {
    val start = Location(startOffset, startRow, startCol)
    val end = Location(endOffset, endRow, endCol)
    (start, end)
  }


  /** Runs the parser on the given input with with the given chunk size. */
  private def runParser(input: String): Json[Attrs] =
    doIO { Json.read(stringInput(input), attrFactory) }
}
