package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException

/** Tests for the literal reader. */
final class LiteralReaderTest extends org.scalatest.funsuite.AnyFunSuite {
  import LiteralReaderTest._
  import Unnest.given

  type IOStream = BufferedLookAhead[java.io.Reader]
  private val reader = LiteralReader.all[Unnest, IOStream]()


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
    testFailure(reader.trueLiteral, "tru", Err("true", 3, -1))
    testFailure(reader.falseLiteral, "fal", Err("false", 3, -1))
    testFailure(reader.nullLiteral, "nul", Err("null", 3, -1))

    testFailure(reader.trueLiteral, "trux", Err("true", 3, 'x'))
    testFailure(reader.falseLiteral, "falx", Err("false", 3, 'x'))
    testFailure(reader.nullLiteral, "nulx", Err("null", 3, 'x'))
  }


  private def testFailure(
        reader: LiteralReader[Unnest, IOStream],
        input: String,
        failure: Err
      ): Unit =  {
    val err = intercept[Err] { runReader(reader, input) }
    assert(err.expected === failure.expected)
    assert(err.offset === failure.offset)
    assert(err.actual === failure.actual)
  }


  private def runReader(reader: LiteralReader[Unnest, IOStream], input: String): Unit =
    Unnest.run(reader(streamFor(input)))


  private def streamFor(source: String): IOStream =
    BufferedLookAhead(new java.io.StringReader(source), 100)


  private given unnestError: BufferedLookAhead.IOErrors[Unnest, java.io.Reader] =
    BufferedLookAhead.IOErrors.raise { [T] => (ctx, msg) => throw new IOException(msg) }


  private given literalError: LiteralReader.Errors[Unnest, IOStream] with {
    override def badLiteral[T](stream: IOStream, expected: String, mismatchOffset: Int, actualChar: Int): Unnest[T] =
      throw new Err(expected, mismatchOffset, actualChar)
  }


  /** Reader for java instances. */
  private given javaReaderReader[M[_]: Monad, T <: java.io.Reader]: Reader[M, T] with {
    override def read(source: T, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      Monad.pure(source.read(target, targetStart, targetEnd - targetStart))
   }
}

object LiteralReaderTest {
  private final case class Err(expected: String, offset: Int, actual: Int) extends IOException
}
