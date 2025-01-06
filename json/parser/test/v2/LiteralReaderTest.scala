package io.github.maxkar
package json.parser.v2

/** Tests for the literal reader. */
final class LiteralReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import TestIO.*
  import TestIO.given

  private val reader = LiteralReader.all[Operation, IOStream]()


  test("Literals are read successfully") {
    runReader(reader.trueLiteral, "true")
    runReader(reader.falseLiteral, "false")
    runReader(reader.nullLiteral, "null")

    runReader(reader.trueLiteral, "true,")
    runReader(reader.falseLiteral, "false,")
    runReader(reader.nullLiteral, "null,")

    runReader(reader.trueLiteral, "true ")
    runReader(reader.falseLiteral, "false ")
    runReader(reader.nullLiteral, "null ")

    runReader(reader.trueLiteral, "truez ")
    runReader(reader.falseLiteral, "falsez")
    runReader(reader.nullLiteral, "nullz")
  }


  test("Invalid literals") {
    testFailure(reader.trueLiteral, "tru", ParseException(0, "Invalid true literal"))
    testFailure(reader.falseLiteral, "fal", ParseException(0, "Invalid false literal"))
    testFailure(reader.nullLiteral, "nul", ParseException(0, "Invalid null literal"))

    testFailure(reader.trueLiteral, "trux", ParseException(0, "Invalid true literal"))
    testFailure(reader.falseLiteral, "falx", ParseException(0, "Invalid false literal"))
    testFailure(reader.nullLiteral, "nulx", ParseException(0, "Invalid null literal"))
  }


  private def testFailure(
        reader: LiteralReader[Operation, IOStream],
        input: String,
        failure: ParseException
      ): Unit =  {
    val err = parseWithError { runReader(reader, input) }
    assert(failure === err)
  }


  private def runReader(reader: LiteralReader[Operation, IOStream], input: String): Unit =
    doIO(reader(stringInput(input)))


  private given literalError: LiteralReader.Errors[Operation, IOStream] =
    LiteralReader.Errors.raise(raise)
}
