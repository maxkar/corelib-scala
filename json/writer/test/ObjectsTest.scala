package io.github.maxkar
package json.writer

import text.output.Stream
import text.output.StringBuilderStream

import fun.instances.Identity
import fun.instances.Identity.given

/** Test for object writers and formatters.  */
final class ObjectsTest extends org.scalatest.funsuite.AnyFunSuite {

  /* Internal spacing implementation. */
  private object spacing extends Objects.Whitespaces[Identity, Stream[Identity]] {
    override def beforeObject(stream: Stream[Identity]): Unit =
      stream.write(" ")
    override def afterObject(stream: Stream[Identity]): Unit =
      stream.write("  ")

    override def insideEmptyObject(stream: Stream[Identity]): Unit =
      stream.write("         ")

    override def beforeFirstKey(stream: Stream[Identity]): Unit =
      stream.write("   ")

    override def beforeKey(stream: Stream[Identity]): Unit =
      stream.write(" ")

    override def afterKey(stream: Stream[Identity]): Unit =
      stream.write("  ")

    override def beforeValue(stream: Stream[Identity]): Unit =
      stream.write("   ")

    override def afterLastValue(stream: Stream[Identity]): Unit =
      stream.write("    ")

    override def afterValue(stream: Stream[Identity]): Unit =
      stream.write("  ")
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
    val stream = StringBuilderStream()
    val w = Objects.newWriter(spacing, Literals.writeBoolean, stream)

    for key <- keys do
      w.entry(key, true)
    w.end()

    assert(expected === stream.data)
  }
}
