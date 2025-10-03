package io.github.maxkar
package json.writer.v2

import text.v2.output.Writer
import text.output.StringBuilderStream

import fun.instances.Identity
import fun.instances.Identity.given

/** Test for object writers and formatters.  */
final class ObjectsTest extends org.scalatest.funsuite.AnyFunSuite {

  /* Internal spacing implementation. */
  private object spacing extends Objects.Whitespaces[Identity, StringBuilder] {
    override def beforeObject(stream: StringBuilder): Unit =
      stream.append(" ")
    override def afterObject(stream: StringBuilder): Unit =
      stream.append("  ")

    override def insideEmptyObject(stream: StringBuilder): Unit =
      stream.append("         ")

    override def beforeFirstKey(stream: StringBuilder): Unit =
      stream.append("   ")

    override def beforeKey(stream: StringBuilder): Unit =
      stream.append(" ")

    override def afterKey(stream: StringBuilder): Unit =
      stream.append("  ")

    override def beforeValue(stream: StringBuilder): Unit =
      stream.append("   ")

    override def afterLastValue(stream: StringBuilder): Unit =
      stream.append("    ")

    override def afterValue(stream: StringBuilder): Unit =
      stream.append("  ")
  }


  test("Empty object") {
    expect(" {         }  ")
  }


  test("One key") {
    expect(""" {   "a"  :   true    }  """, "a")
  }

  test("Two keys") {
    expect(""" {   "a"  :   true  , "b"  :   true    }  """, "a", "b")
  }

  test("Three keys") {
    expect(""" {   "a"  :   true  , "b"  :   true  , "c"  :   true    }  """, "a", "b", "c")
  }


  private def expect(expected: String, keys: String*): Unit = {
    val stream = StringBuilder()
    val w = Objects.newWriter(spacing, Literals.writeBoolean, stream)

    for key <- keys do
      w.entry(key, true)
    w.end()

    assert(expected === stream.toString())
  }
}
