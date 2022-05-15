package io.github.maxkar
package json.parser

/** Basic qualitatitve (parses or not) parsing test. */
final class QualitativeParsingTest extends org.scalatest.funsuite.AnyFunSuite:
  import QualitativeParser.given

  /** Parser we use for test. */
  private val parser =
    val imputImpl =
      input.ParserInput.fromBasic(
        QualitativeParser.Input,
        [T] => () => QualitativeParser.fail
      )
    new JsonParser(imputImpl, QualitativeParser.Factory)


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


  /** Checks that parsing is successfull .*/
  private def parseSucc(input: String): Unit =
    withClue(input) {
      QualitativeParser.runMd(input, parser.parseValue) match {
        case (pos, None) => fail(s"Expected to succeed, failed at pos ${pos}")
        case (pos, Some(x)) if pos != input.length => fail(s"Expected to consume all string, finished at ${pos}")
        case _ => ()
      }
    }


  /** Checks that parsing is unsuccessful. */
  private def parseFail(input: String, failPos: Int): Unit =
    withClue(input) {
      QualitativeParser.runMd(input, parser.parseValue) match {
        case (pos, Some(_)) => fail(s"Expected to fail but succeeded at ${pos}")
        case (`failPos`, _) => ()
        case (pos, _) => fail(s"Expected to fail at ${failPos} but failed at ${pos}")
      }
    }
end QualitativeParsingTest
