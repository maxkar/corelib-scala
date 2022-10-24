package io.github.maxkar
package json.writer

import text.output.Stream

import fun.instances.Identity
import fun.instances.Identity.given

/** Test for array writers and formatters.  */
final class ArraysTest extends org.scalatest.funsuite.AnyFunSuite:

  /* Internal spacing implementation. */
  private object spacing extends Arrays.Whitespaces[Identity, Stream[Identity]]:
    override def beforeArray(stream: Stream[Identity]): Unit =
      stream.write(" ")
    override def afterArray(stream: Stream[Identity]): Unit =
      stream.write("  ")

    override def insideEmptyArray(stream: Stream[Identity]): Unit =
      stream.write("         ")

    override def beforeFirstValue(stream: Stream[Identity]): Unit =
      stream.write("   ")

    override def beforeValue(stream: Stream[Identity]): Unit =
      stream.write(" ")

    override def afterLastValue(stream: Stream[Identity]): Unit =
      stream.write("    ")

    override def afterValue(stream: Stream[Identity]): Unit =
      stream.write("  ")
  end spacing


  test("Empty array") {
    expect(" [         ]  ", 0)
  }

  test("One element") {
    expect(" [   true    ]  ", 1)
  }

  test("Two elements") {
    expect(" [   true  , true    ]  ", 2)
  }

  test("Three elements") {
    expect(" [   true  , true  , true    ]  ", 3)
  }


  private def expect(expected: String, elems: Int): Unit =
    val stream = SimpleStream()
    val w = Arrays.newWriter(spacing, Literals.writeBoolean, stream)

    var idx = elems
    while idx > 0 do
      w.element(true)
      idx -= 1
    end while
    w.end()

    assert(expected === stream.data)
  end expect
end ArraysTest
