package io.github.maxkar
package json.query

import scala.language.implicitConversions

final class PathTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Basic path equalities and properties hold") {
    assert(Path.empty === Path.empty)
    assert(Path.root === Path.empty)

    assert(Path.empty.isEmpty)
    assert(!Path.empty.nonEmpty)

    assert(Path("abc") === Path("abc"))
    assert(Path(123) === Path(123))

    assert(Path("abc").nonEmpty)
    assert(!Path("abc").isEmpty)

    assert(Path("abc", 123) === Path("abc") / 123)
    assert(Path("abc", 123) === Path("abc") + Path(123))
    assert(Path(123, "abc") === Path(123) / "abc")
    assert(Path(123, "abc") === Path(123) + Path("abc"))
  }


  test("Path is converted to string as expected") {
    assert(Path.root.toString() === "<root>")
    assert(Path("abc").toString() === "abc")
    assert(Path(123).toString() === "[123]")

    assert(Path("abc", "def").toString() === "abc.def")
    assert(Path(123, 456).toString() === "[123][456]")

    assert(Path("abc", 456).toString() === "abc[456]")
    assert(Path(123, "def").toString() === "[123].def")

    assert(Path("").toString() === "[\"\"]")
    assert(Path(" ").toString() === "[\" \"]")
    assert(Path("cat\"").toString() === "[\"cat\\\"\"]")
  }
end PathTest
