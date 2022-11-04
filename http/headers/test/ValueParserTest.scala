package io.github.maxkar
package http.headers

/** Test for the header value parser. */
final class ValueParserTest extends org.scalatest.funsuite.AnyFunSuite:

  test("Reading tokens") {
    check("hello", "hello")(_.readToken())
    check("hello", "hello,")(_.readToken())
    check("***", "***")(_.readToken())
    check("***", "***///")(_.readToken())

    checkFail(0, "\"hello\"")(_.readToken())
    checkFail(0, ",hello")(_.readToken())
  }


  test("Reading strings") {
    check("hello", "\"hello\"")(_.readString())
    check("hello", "\"hello\",")(_.readString())

    check("hel\\lo", "\"hel\\\\lo\"")(_.readString())
    check("hel\\lo", "\"hel\\\\lo\",")(_.readString())

    check("\\hello", "\"\\\\hello\"")(_.readString())
    check("\\hello", "\"\\\\hello\",")(_.readString())
    check("hello\\", "\"hello\\\\\"")(_.readString())
    check("hello\\", "\"hello\\\\\",")(_.readString())

    checkFail(0, "hello")(_.readString())
    checkFail(0, ",hello")(_.readString())
    checkFail(6, "\"hello")(_.readString())
    checkFail(4, "\"hel\u0005lo\"")(_.readString())
  }


  test("Reading parameters") {
    check(Seq("a" -> "b", "c" -> "d"), ";a=b;c=d")(_.readParameters())
    check(Seq("a" -> "b", "c" -> "d"), ";a=\"b\";c=\"d\"")(_.readParameters())
    check(Seq("a" -> "b", "c" -> "d"), " ; a=b  ;  c=d  ")(_.readParameters())
    check(Seq("a" -> "b", "c" -> "d"), " ; a=\"b\"  ;  c=\"d\"  ")(_.readParameters())
    check(Seq("a" -> "b", "c" -> "d"), ";a=b;c=d,")(_.readParameters())
    check(Seq("a" -> "b", "c" -> "d"), ";a=\"b\";c=\"d\",")(_.readParameters())
    check(Seq("a" -> "b", "c" -> "d"), " ; a=b  ;  c=d  ,  ")(_.readParameters())
    check(Seq("a" -> "b", "c" -> "d"), " ; a=\"b\"  ;  c=\"d\"  ,  ")(_.readParameters())
    check(Seq.empty, "")(_.readParameters())
    check(Seq.empty, ",")(_.readParameters())

    checkFail(1, ";")(_.readParameters())
    checkFail(2, ";a")(_.readParameters())
    checkFail(3, ";a=")(_.readParameters())
    checkFail(3, ";a={}")(_.readParameters())
    checkFail(2, ";a{}")(_.readParameters())
  }


  test("Reading first element of the list") {
    check(false, "")(_.hasFirstListElement())
    check(false, "   ")(_.hasFirstListElement())
    check(true, "23")(_.hasFirstListElement())
    check(true, ",23")(_.hasFirstListElement())
    check(true, "  23")(_.hasFirstListElement())
    check(true, "  ,   23")(_.hasFirstListElement())
    checkFail(8, "  ,   , ")(_.hasFirstListElement())
    checkFail(2, ",,")(_.hasFirstListElement())
    checkFail(5, ",,,,,")(_.hasFirstListElement())
  }


  test("Reading next element of the list") {
    check(false, "")(_.hasNextListElement())
    check(false, "  ")(_.hasNextListElement())
    check(false, "  , , ,   , ")(_.hasNextListElement())
    check(false, "  , , ,   , ")(_.hasNextListElement())
    check(true, ", aoe ")(_.hasNextListElement())

    checkFail(0, "test")(_.hasNextListElement())
    checkFail(4, "    test")(_.hasNextListElement())
  }


  /** Checks parsing of the given string. */
  def check[T](expected: T, input: String)(parse: ValueParser => T): Unit =
    withClue(input) {
      val parser = new ValueParser(input)
      assert(expected === parse(parser))
    }


  /** Checks that parsing fails for the given string. */
  def checkFail(failOffset: Int, input: String)(parse: ValueParser => _): Unit =
    withClue(input) {
      val parser = new ValueParser(input)
      try
        parse(parser)
        fail("Expected header format exception, got nothing")
      catch
        case e: HeaderFormatException => ()
      assert(parser.offset === failOffset)
    }



  /** Checks that the given result denotes failure. */
  private def assertFail(x: Either[String, _]): Unit =
    x match
      case Left(_) => ()
      case Right(x) => fail(s"Expected error but got success ${x}")
  end assertFail
end ValueParserTest
