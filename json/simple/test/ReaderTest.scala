package io.github.maxkar
package json.simple

import fun.instances.Identity
import fun.instances.Identity.given

import java.nio.CharBuffer

import text.input.LookAheadStream
import text.input.BufferLookAheadStream

import json.parser.Values.AllErrors
import json.parser.Errors



/** Tests for simple json parsing. */
class AttributeParsingTest extends org.scalatest.funsuite.AnyFunSuite:
  /** Simple implementation of the error handler. */
  private object RaiseError extends Errors.SimpleHandler[Identity, Any]:
    override def raise[T](stream: Any, message: String): T =
      throw new java.io.IOException(message)
  end RaiseError


  /** Error handler for all the errors. */
  given errorHandler: Errors.ErrorHandler[Identity, LookAheadStream[Identity]] =
    Errors.simple[Identity, LookAheadStream[Identity]](RaiseError)
  import errorHandler.endOfFileErrors


  test("Some basic literals work") {
    assert(parse("true") === Json.True)
    assert(parse("false") === Json.False)
    assert(parse("null") === Json.Null)
  }


  test("Number parsing works") {
    assert(parse("1") === Json.Number("1"))
    assert(parse("1.25") === Json.Number("1.25"))
    assert(parse("1.25E+5") === Json.Number("1.25E+5"))
  }


  test("String parsing works") {
    assert(parse("\"a\"") === Json.String("a"))
    assert(parse("\"abc\"") === Json.String("abc"))
    assert(parse("\"a\\u0020c\"") === Json.String("a c"))
  }


  test("Array parsing works") {
    assert(parse("[]") === Json.Array(Seq.empty))
    assert(parse("[true]") === Json.Array(Seq(Json.True)))

    assert(parse("[true,\n false\n]") ===
      Json.Array(Seq(Json.True, Json.False))
    )
  }


  test("Object parsing works") {
    assert(parse("{}") === Json.Object(Map.empty))

    val object1 = Json.Object(Map("vl" -> Json.True))

    val object2 = Json.Object(
      Map(
        "vl" -> Json.True,
        "vv" -> Json.False,
      )
    )

    assert(parse("""{"vl": true}""") === object1)
    assert(parse("{\n  \"vl\": true,\n  \"vv\": false\n}") === object2)
  }


  /** Parses the input. */
  private def parse(input: String): Json =
    val reader = new java.io.StringReader(input)
    val filler = BufferLookAheadStream.Filler[Identity](reader, (), x => throw x)
    val inputStream = BufferLookAheadStream(filler, CharBuffer.allocate(10))
    Json.read(inputStream)
  end parse
end AttributeParsingTest
