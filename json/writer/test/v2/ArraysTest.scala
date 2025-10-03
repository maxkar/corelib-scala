package io.github.maxkar
package json.writer.v2

import text.v2.output.Writer

import fun.instances.Identity
import fun.instances.Identity.given

/** Test for array writers and formatters.  */
final class ArraysTest extends org.scalatest.funsuite.AnyFunSuite {

  /* Internal spacing implementation. */
  private object spacing extends Arrays.Whitespaces[Identity, StringBuilder] {
    override def beforeArray(stream: StringBuilder): Unit =
      stream.append(" ")
    override def afterArray(stream: StringBuilder): Unit =
      stream.append("  ")

    override def insideEmptyArray(stream: StringBuilder): Unit =
      stream.append("         ")

    override def beforeFirstValue(stream: StringBuilder): Unit =
      stream.append("   ")

    override def beforeValue(stream: StringBuilder): Unit =
      stream.append(" ")

    override def afterLastValue(stream: StringBuilder): Unit =
      stream.append("    ")

    override def afterValue(stream: StringBuilder): Unit =
      stream.append("  ")
  }


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


  private def expect(expected: String, elems: Int): Unit = {
    val stream = StringBuilder()
    val w = Arrays.newWriter(spacing, Literals.writeBoolean, stream)

    var idx = elems
    while idx > 0 do {
      w.element(true)
      idx -= 1
    }
    w.end()

    assert(expected === stream.toString())
  }
}
