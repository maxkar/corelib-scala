package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest
import java.io.IOException


/** Tests for the whitespace reader. */
final class WhitespaceReaderTest extends org.scalatest.funsuite.AnyFunSuite {

  test("Whitespaces are read skipped as expected") {
    checkRead("  Hello, world", "  ")
    checkRead("   \t\r\n  Wr", "   \t\r\n  ")
    checkRead("   Test", "   ")
    checkRead("    ", "    ")
    checkRead("X X", "")
    checkRead("", "")
  }


  private def checkRead(input: String, expected: String): Unit =
    assert(expected === read(input))


  private def read(source: String): String = {
    import Unnest.given

    val sr = new java.io.StringReader(source)
    val br = BufferedLookAhead(sr, 100)
    val wsr = WhitespaceReader(br)

    Unnest.run(wsr.readString())
  }

  private given unnestError: BufferedLookAhead.IOErrors[Unnest] with {
    override def lookAheadTooBig[T](requested: Int, supported: Int): Unnest[T] =
      throw new IOException(
        s"Look ahead ${requested} is greater than the supported amount of ${supported}"
      )
  }

  /** Reader for java instances. */
  private given javaReaderReader[M[_]: Monad, T <: java.io.Reader]: Reader[M, T] with {
    override def read(source: T, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      Monad.pure(source.read(target, targetStart, targetEnd - targetStart))
   }
}

