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


  private given unnestError: BufferedLookAhead.IOErrors[Unnest, java.io.Reader] =
    BufferedLookAhead.IOErrors.raise { [T] => (ctx, msg) => throw new IOException(msg) }

  /** Reader for java instances. */
  private given javaReaderReader[M[_]: Monad]: Reader[M, java.io.Reader] with {
    override def read(source: java.io.Reader, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      Monad.pure(source.read(target, targetStart, targetEnd - targetStart))
   }
}

