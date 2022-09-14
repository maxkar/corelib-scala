package io.github.maxkar
package json.simple.factory

import json.simple.Json

import json.parser.JsonParser

import json.parser.chunky.Module
import json.parser.chunky.SourceLocation


/** Tests for simple parsing. */
class AttributeParsingTest extends org.scalatest.funsuite.AnyFunSuite:
  /** Parsing module & operations. */
  private val module = Module[String](unexpectedEof = loc => s"${loc}: Unexpected EOF")
  import module.given


  /** Factory of the json values. */
  private val factory =
    new Factory[module.Parser](
      fail = msg => module.abort(msg),
    )

  /** Parser to use in tests. */
  private val parser = new JsonParser(parserInput, factory)


  test("Some basic literals work") {
    assert(runParser("true") === Right(Json.True))
    assert(runParser("false") === Right(Json.False))
    assert(runParser("null") === Right(Json.Null))
  }


  test("Number parsing works") {
    assert(runParser("1") === Right(Json.Number("1")))
    assert(runParser("1.25") === Right(Json.Number("1.25")))
    assert(runParser("1.25E+5") === Right(Json.Number("1.25E+5")))
  }


  test("String parsing works") {
    assert(runParser("\"a\"") === Right(Json.String("a")))
    assert(runParser("\"abc\"") === Right(Json.String("abc")))
    assert(runParser("\"a\\u0020c\"") === Right(Json.String("a c")))
  }


  test("Array parsing works") {
    assert(runParser("[]") === Right(Json.Array(Seq.empty)))
    assert(runParser("[true]") === Right(Json.Array(Seq(Json.True))))

    assert(runParser("[true,\n false\n]") === Right(
      Json.Array(Seq(Json.True, Json.False))
    ))
  }


  test("Object parsing works") {
    assert(runParser("{}") === Right(Json.Object(Map.empty)))

    val object1 = Json.Object(Map("vl" -> Json.True))

    val object2 = Json.Object(
      Map(
        "vl" -> Json.True,
        "vv" -> Json.False,
      )
    )

    assert(runParser("""{"vl": true}""") === Right(object1))
    assert(runParser("{\n  \"vl\": true,\n  \"vv\": false\n}") === Right(object2))
  }


  /** Runs the parser on the given input with with the given chunk size. */
  private def runParser(input: String): Either[String, Json] =
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


