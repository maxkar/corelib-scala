package io.github.maxkar
package json.parser.v3

import fun.instances.Unnest
import TestIO.*

final class LiteralsTest extends org.scalatest.funsuite.AnyFunSuite {
  private object Factory extends Literals.Factory[Operation, JsonStream, Unit] {
    override def read(stream: JsonStream, count: Int): Operation[Unit] =
      stream.skip(count)

    override def invalidLiteral(stream: JsonStream, expected: String): Operation[Unit] =
      raise(stream, s"Bad literal ${expected}")
  }


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
    testFailure("tru", "Bad literal true", Literals.readTrue)
    testFailure("fal", "Bad literal false", Literals.readFalse)
    testFailure("nul", "Bad literal null", Literals.readNull)

    testFailure("trux", "Bad literal true", Literals.readTrue)
    testFailure("falx", "Bad literal false", Literals.readFalse)
    testFailure("nulx", "Bad literal null", Literals.readNull)
  }


  private def testSuccess(
        input: String,
        length: Int,
        reader: (JsonStream, Factory.type) => Operation[Unit]
      ): Unit =
    withClue(input) {
      assert(length === parse(input, reader(_, Factory))._2)
    }


  private def testFailure(
        input: String,
        message: String,
        reader: (JsonStream, Factory.type) => Operation[Unit]
      ): Unit =
    withClue(input) {
      val exn = failParse(input, reader(_, Factory))
      assert(0 === exn.offset)
      assert(message === exn.message)
    }

}

