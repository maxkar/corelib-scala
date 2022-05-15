package io.github.maxkar
package json.parser.chunky

import json.parser.JsonParser

/**
 * Basic qualitatitve (parses or not) parsing test using chunky machinery.
 * Tests parisng various sizes of input chunks. Validates error location reporting
 * (this is available form the module).
 */
final class ChunkyParserTest extends org.scalatest.funsuite.AnyFunSuite:
  /** Parsing module & operations. */
  private val module = Module[SourceLocation](unexpectedEof = loc => loc)
  import module.given

  /** Factory of the json values. */
  private val factory = new ChunkyFactory[module.Parser](module.location.flatMap(module.abort))

  /** Parser to use in tests. */
  private val parser = new JsonParser(parserInput, factory)


  test("Parsing basic literals works") {
    parseSucc("true")
    parseSucc("false")
    parseSucc("null")
  }


  test("Parsing things that looks like literal fails") {
    parseFail("truffel", 3)
    parseFail("falafel", 3)
    parseFail("nuts", 2)
  }


  test("Parsing numbers works") {
    val nonEmptyExps =
      for
        indicator <- Seq("E", "e")
        sign <- Seq("", "+", "-")
        digits <- Seq("23", "1", "885")
      yield s"${indicator}${sign}${digits}"

    val exps = "" +: nonEmptyExps

    for
      sign <- Seq("", "-")
      lead <- Seq("236", "0", "5")
      frac <- Seq("", ".435", ".8", ".0001")
      exp <- exps
    do parseSucc(s"${sign}${lead}${frac}${exp}")
  }


  test("Parsing bad numbers does not work") {
    parseFail("-", 1)
    parseFail("-.", 1)
    parseFail("-E", 1)
    parseFail("00.2235", 1)
    parseFail("-00.225", 2)
    parseFail("23.", 3)
    parseFail("23.E8", 3)
    parseFail("23.235E", 7)
    parseFail("23.235Exe", 7)
    parseFail("23.235E+", 8)
    parseFail("23.235E-", 8)
    parseFail("23.235E+some", 8)
    parseFail("23.235E-some", 8)
  }


  test("String parsing works") {
    parseSucc("\"\"")
    parseSucc("\"Hello, World\"")
    parseSucc("\"Hello, escapes: \\r\\n\\t\\\" \\u0001\\u0058 \"")
  }


  test("Parsing malformed strings does not work") {
    parseFail("\"", 1)
    parseFail("\"  \\a\"", 4)
    /* Unterminated string. */
    parseFail("\"  \\\"", 5)
    parseFail("\"  \\u\"", 5)
    parseFail("\"  \\uG\"", 5)
    parseFail("\"  \\u0G\"", 6)
    parseFail("\"  \\u05G\"", 7)
    parseFail("\"  \\u058G\"", 8)
    parseFail("\"  \r\"", 3)
    parseFail("\"  \n\"", 3)
  }


  test("Array parsing works") {
    parseSucc("[]")
    parseSucc("[true]")
    parseSucc("[   true, false , null ]")
    parseSucc("[[[[]]]]")
  }


  test("Parsing bad arrays fails") {
    parseFail("[,", 1)
    parseFail("[true, ]", 7)
    parseFail("[true: ]", 5)
    parseFail("[true ", 6)
  }


  test("Object parsing works") {
    parseSucc("{}")
    parseSucc("""{"k": true}""")
    parseSucc("""{"true": true, "false": false,   "null"  :  null  }""")
  }


  test("Parsing bad objects fails") {
    parseFail("{  ", 3)
    parseFail("""{ "key" """, 8)
    parseFail("""{ "key": """, 9)
    parseFail("""{ "key": 25""", 11)
    parseFail("""{ "key": 25 and "value": 51} """, 12)
    parseFail("""{ "key": 25 , 12: "value"} """, 14)
    parseFail("""{ , } """, 2)
  }


  test("Some complex thing") {
    parseSucc("""  { "k": [{"a": 7}, null, {"b": {}}] }""")
  }


  test("Parsing non-values fails") {
    parseFail("Cat", 0)
    parseFail("<>", 0)
    parseFail("//", 0)
  }


  test("Deep nesting does not cause any issues") {
    val depth = 100000
    /* Deeply-nested string .*/
    val deepNest = ("[" * depth) + ("]" * depth)

    /* Go with some manual lengths The default "run succ" tries all lengths
     * which gives a N^2 run time where N is the total string length.
     */
    assert(runParser(deepNest, 1) === None)
    assert(runParser(deepNest, 1024) === None)
    assert(runParser(deepNest, 2048) === None)
    assert(runParser(deepNest, deepNest.length) === None)
  }


  test("Deep nesting correctly handles errors in the middle of nesting") {
    val depth = 100000
    /* Deeply-nested string .*/
    val deepNest = ("[" * depth) + "*" + ("]" * depth)

    /* Go with some manual lengths The default "run succ" tries all lengths
     * which gives a N^2 run time where N is the total string length.
     */
    assert(runParser(deepNest, 1) === Some(SourceLocation(depth, 1, depth + 1)))
    assert(runParser(deepNest, 1024) === Some(SourceLocation(depth, 1, depth + 1)))
    assert(runParser(deepNest, 2048) === Some(SourceLocation(depth, 1, depth + 1)))
    assert(runParser(deepNest, deepNest.length) === Some(SourceLocation(depth, 1, depth + 1)))
  }


  test("Deep nesting correctly handles errors ad the end of nesting") {
    val depth = 100000
    /* Deeply-nested string .*/
    val deepNest = ("[" * depth) + ("]" * (depth - 1))

    /* Go with some manual lengths The default "run succ" tries all lengths
     * which gives a N^2 run time where N is the total string length.
     */
    assert(runParser(deepNest, 1) === Some(SourceLocation(2 * depth - 1, 1, 2 * depth)))
    assert(runParser(deepNest, 1024) === Some(SourceLocation(2 * depth - 1, 1, 2 * depth)))
    assert(runParser(deepNest, 2048) === Some(SourceLocation(2 * depth - 1, 1, 2 * depth)))
    assert(runParser(deepNest, deepNest.length) === Some(SourceLocation(2 * depth - 1, 1, 2 * depth)))
  }

  /** Checks that parsing is successfull .*/
  private def parseSucc(input: String): Unit =
    withClue(input) {
      for (chunkSize <- 1 to input.length) {
        withClue(s"Chunk ${chunkSize}") {
          assert(runParser(input, chunkSize) === None)
        }
      }
    }


  /** Checks that parsing is unsuccessful assuming single-line input. */
  private def parseFail(input: String, failOffset: Int): Unit =
    parseFail(input, SourceLocation(failOffset, 1, failOffset + 1))


  /** Checks that parsing is unsuccessful. */
  private def parseFail(input: String, failPos: SourceLocation): Unit =
    withClue(input) {
      for (chunkSize <- 1 to input.length) {
        withClue(s"Chunk ${chunkSize}") {
          assert(runParser(input, chunkSize) === Some(failPos))
        }
      }
    }


  /** Runs the parser on the given input with with the given chunk size. */
  private def runParser(input: String, chunkSize: Int): Option[SourceLocation] =
    val consumer = module.start(parser.parseValue)

    var pos = 0
    while pos < input.length do
      val nextChunkSize = Math.min(input.length - pos, chunkSize)
      val consumed = consumer.process(input.subSequence(pos, pos + nextChunkSize))
      /* Consumer indicated that it does not want any more input. */
      if consumed < nextChunkSize then
        return convertResult(consumer.end())

      pos += consumed
    end while

    convertResult(consumer.end())
  end runParser


  /** Converts parsing result into test result. */
  private def convertResult(base: Either[SourceLocation, Unit]): Option[SourceLocation] =
    base match {
      case Left(err) => Some(err)
      case Right(succ) => None
    }
end ChunkyParserTest
