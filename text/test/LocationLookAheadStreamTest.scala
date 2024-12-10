package io.github.maxkar
package text.input

import fun.typeclass.Monad
import fun.instances.Identity
import fun.instances.Identity.given

import java.io.StringReader
import java.nio.CharBuffer

import text.Location

/**
 * Tests for location look-ahead stream.
 */
final class LocationLookAheadStreamTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Location is tracked correctly (smoke test)") {
    val reader = new StringReader("Hello\n world\rThird line\r\nAnd the pair")
    val buffer = CharBuffer.allocate(10)
    val base =
      BufferLookAheadStream(
        BufferLookAheadStream.Filler(reader, (): Identity[Unit], err => throw err),
        buffer
      )

    val loc = LocationLookAheadStream(base, ())
    assert(loc.location === Location(0, 1, 1))

    loc.peek(6)
    assert(loc.location === Location(0, 1, 1))
    assert(loc.consume(4).toString() === "Hell")
    assert(loc.location === Location(4, 1, 5))

    loc.peek(2)
    loc.skip(2)
    assert(loc.location === Location(6, 2, 1))

    loc.peek(8)
    assert(loc.consume(3).toString === " wo")
    assert(loc.location === Location(9, 2, 4))
    loc.peek(8)
    assert(loc.consume(3).toString === "rld")
    assert(loc.location === Location(12, 2, 7))

    loc.peek(1)
    loc.skip(1)
    assert(loc.location === Location(13, 3, 1))

    loc.peek(5)
    assert(loc.consume(5).toString() === "Third")
    assert(loc.location === Location(18, 3, 6))

    loc.peek(10)
    loc.skip(10)
    assert(loc.location === Location(28, 4, 4))
  }
}
