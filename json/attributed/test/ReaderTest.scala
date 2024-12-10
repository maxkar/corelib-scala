package io.github.maxkar
package json.attr

import fun.instances.Identity
import fun.instances.Identity.given

import java.nio.CharBuffer

import text.Location

import text.input.LookAheadStream
import text.input.BufferLookAheadStream
import text.input.LocationLookAheadStream

import json.parser.Values.AllErrors
import json.parser.Errors
import Json.ObjectEntry


/** Tests for attributed parsing reader. */
class ReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  /** Attributes of the resulting json. */
  type Attrs = (Location, Location)

  /** Factory for the attributes. */
  private val attrFactory = AttributeFactory.span

  /** Simple implementation of the error handler. */
  private object RaiseError extends Errors.SimpleHandler[Identity, LocationLookAheadStream[Identity, Any]] {
    override def raise[T](stream: LocationLookAheadStream[Identity, Any], message: String): T =
      throw new java.io.IOException(s"${stream.location}: ${message}")
  }


  /** Error handler for all the errors. */
  given errorHandler: Errors.ErrorHandler[Identity, LocationLookAheadStream[Identity, Any]] =
    Errors.simple[Identity, LocationLookAheadStream[Identity, Any]](RaiseError)
  import errorHandler.endOfFileErrors

  /** Attribute-specific errors. */
  given attrErrors: Reader.Errors[Identity, LocationLookAheadStream[Identity, Any], Attrs] with {
    override def duplicateObjectKey(
          prevEntry: ObjectEntry[Attrs],
          newKeyAttrs: Attrs,
          stream: LocationLookAheadStream[Identity, Any],
        ): Identity[Unit] =
      throw new java.io.IOException(
        s"${newKeyAttrs._1}: Duplicate key ${prevEntry.key}, previous definition at ${prevEntry.keyAttrs._1}"
      )
  }


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
  private def runParser(input: String): Json[Attrs] = {
    val reader = new java.io.StringReader(input)
    val filler = BufferLookAheadStream.Filler[Identity](reader, (), x => throw x)
    val baseInputStream = BufferLookAheadStream(filler, CharBuffer.allocate(10))
    val inputStream = LocationLookAheadStream(baseInputStream)
    Json.read(inputStream, attrFactory)
  }
}
