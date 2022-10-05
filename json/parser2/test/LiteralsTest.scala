package io.github.maxkar
package json.parser

/**
 * Tests for the literal parser.
 */
final class LiteralsTest extends org.scalatest.funsuite.AnyFunSuite {
  import LiteralsTest._

  given Literals.Errors[Identity] with
    override def badLiteral(expected: String, actual: CharSequence): Unit =
      throw new BadLiteral(expected, actual.toString())
  end given

  test("isSameLiteral works") {
    assert(Literals.isSameLiteral("hello", "hello"))
    assert(Literals.isSameLiteral("hello", "helloo"))
    assert(!Literals.isSameLiteral("hello", "hell"))
  }

  test("startsWithLiteral works") {
    input.SimpleStringStream.forAllLookAheadSizes[Identity]("hello, world") { stream =>
      assert(Literals.startsWithLiteral("hello", stream))
      assert(stream.readOffset === 0)
    }
  }


  test("readLiteral works") {
    input.SimpleStringStream.forAllLookAheadSizes[Identity]("hello, world") { stream =>
      Literals.readLiteral("hello", stream)
      assert(stream.readOffset === 5)
    }
  }

  test("readLiteral properly handles invalid terminals") {
    input.SimpleStringStream.forAllLookAheadSizes[Identity]("greeting") { stream =>
      try
        Literals.readLiteral("greeter", stream)
      catch
        case BadLiteral("greeter", "greetin") => ()
        case other => throw other
      end try
      assert(stream.readOffset === 0)
    }
  }


  test("readTrue works") {
    input.SimpleStringStream.forAllLookAheadSizes[Identity]("true?") { stream =>
      Literals.readTrue(stream)
      assert(stream.readOffset === 4)
    }
  }


  test("readFalse works") {
    input.SimpleStringStream.forAllLookAheadSizes[Identity]("false?") { stream =>
      Literals.readFalse(stream)
      assert(stream.readOffset === 5)
    }
  }


  test("readNull works") {
    input.SimpleStringStream.forAllLookAheadSizes[Identity]("null?") { stream =>
      Literals.readNull(stream)
      assert(stream.readOffset === 4)
    }
  }
}


private object LiteralsTest:
  /** Encoding of the "bad literal" exception. */
  final case class BadLiteral(expected: String, actual: String) extends Exception
end LiteralsTest