package io.github.maxkar
package json.parser

import TestIO.*
import TestIO.given

final class LiteralsTest extends org.scalatest.funsuite.AnyFunSuite {

  private val factory = new Literals.Factory.Simple(())

  test("Literals are read successfully") {
    testSuccess("true", 4, Literals.readTrue)
    testSuccess("false", 5, Literals.readFalse)
    testSuccess("null", 4, Literals.readNull)

    testSuccess("true,", 4, Literals.readTrue)
    testSuccess("false,", 5, Literals.readFalse)
    testSuccess("null,", 4, Literals.readNull)

    testSuccess("true ", 4, Literals.readTrue)
    testSuccess("false ", 5, Literals.readFalse)
    testSuccess("null ", 4, Literals.readNull)

    testSuccess("truez ", 4, Literals.readTrue)
    testSuccess("falsez ", 5, Literals.readFalse)
    testSuccess("nullz ", 4, Literals.readNull)
  }


  test("Invalid literals") {
    testFailure("tru", "Invalid true literal", Literals.readTrue)
    testFailure("fal", "Invalid false literal", Literals.readFalse)
    testFailure("nul", "Invalid null literal", Literals.readNull)

    testFailure("trux", "Invalid true literal", Literals.readTrue)
    testFailure("falx", "Invalid false literal", Literals.readFalse)
    testFailure("nulx", "Invalid null literal", Literals.readNull)
  }


  private def testSuccess(
        input: String,
        length: Int,
        reader: factory.type => JsonStream => Operation[Unit]
      ): Unit =
    withClue(input) {
      assert(length === parse(input, reader(factory))._2)
    }


  private def testFailure(
        input: String,
        message: String,
        reader: factory.type => JsonStream => Operation[Unit]
      ): Unit =
    withClue(input) {
      val exn = failParse(input, reader(factory))
      assert(0 === exn.offset)
      assert(message === exn.message)
    }

}

