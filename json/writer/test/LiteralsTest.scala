package io.github.maxkar
package json.writer

import text.output.Stream

import fun.instances.Identity
import fun.instances.Identity.given

/** Test for literal writers.  */
final class LiteralsTest extends org.scalatest.funsuite.AnyFunSuite:
  test("true") {
    expect("true", Literals.writeTrue)
  }

  test("false") {
    expect("false", Literals.writeFalse)
  }

  test("null") {
    expect("null", Literals.writeNull)
  }


  test("writeBoolean") {
    expect("true", Literals.writeBoolean(true, _))
    expect("false", Literals.writeBoolean(false, _))
  }

  private def expect(expected: String, writer: Stream[Identity] => Unit): Unit =
    val stream = SimpleStream()
    writer(stream)
    assert(expected === stream.data)
  end expect
end LiteralsTest
