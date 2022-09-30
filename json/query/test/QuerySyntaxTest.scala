package io.github.maxkar
package json.query

import scala.language.implicitConversions

/**
 * Tests for Json query and corresponding syntax extensions.
 */
final class QuerySyntaxTest extends org.scalatest.funsuite.AnyFunSuite:
  import QuerySyntaxTest._
  import QuerySyntaxTest.given
  import Model._

  test("Basic navigation works") {
    val a = Primitive

    val arrA = Array(Seq(a, a, a))
    val arrB = Array(Seq(a, arrA, a))

    val objA = Object(Map("aA" -> arrA, "Ab" -> arrB, "x" -> a, " " -> a))
    val objB = Object(Map(" | " -> objA))

    val arrC = Array(Seq(objA, objB))
    val objC = Object(Map("fld" -> arrC))

    assert(Query(a) === Query.ValidQuery(Path.empty, a))
    assert(Query(arrA) === Query.ValidQuery(Path.empty, arrA))

    assert(Query(a).x === Query.InvalidSelector(Path.empty, a, Path("x")))
    assert(Query(a)(25) === Query.InvalidSelector(Path.empty, a, Path(25)))

    assert(Query(arrA).v === Query.InvalidSelector(Path.empty, arrA, Path("v")))
    assert(Query(arrA)(0) === Query.ValidQuery(Path(0), a))
    assert(Query(arrA)(-1) === Query.MissingElement(Path.empty, arrA, Path(-1)))
    assert(Query(arrA)(3) === Query.MissingElement(Path.empty, arrA, Path(3)))
    assert(Query(arrA).test === Query.InvalidSelector(Path.empty, arrA, Path("test")))

    assert(Query(objA).aA === Query.ValidQuery(Path("aA"), arrA))
    assert(Query(objA).Ab === Query.ValidQuery(Path("Ab"), arrB))
    assert(Query(objA).miss === Query.MissingElement(Path.empty, objA, Path("miss")))
    assert(Query(objA).aA(0) === Query.ValidQuery(Path("aA", 0), a))
    assert(Query(objA).aA(20) === Query.MissingElement(Path("aA"), arrA, Path(20)))

    assert(Query(objB)(" | ").aA(2) === Query.ValidQuery(Path(" | ", "aA", 2), a))
    assert(Query(objC).fld(1)(" | ").Ab(0) === Query.ValidQuery(Path("fld", 1, " | ", "Ab", 0), a))
    assert(Query(objC).fld(1, " | ").Ab(0) === Query.ValidQuery(Path("fld", 1, " | ", "Ab", 0), a))
    assert(Query(objC)("fld", 1, " | ").Ab(0) === Query.ValidQuery(Path("fld", 1, " | ", "Ab", 0), a))
  }
end QuerySyntaxTest


object QuerySyntaxTest:
  enum Model:
    /** Primitive (unspecified) value. */
    case Primitive
    /** Array of elements. */
    case Array(elems: Seq[Model])
    /** Object model. */
    case Object(elems: Map[String, Model])
  end Model

  given ModelNavigation[Model] with
    override def index(base: Model, index: Int): ModelStepResult[Model] =
      base match
        case Model.Array(elems) =>
          if 0 <= index && index < elems.length
          then ModelStepResult.Success(elems(index))
          else ModelStepResult.MissingValue
        case _ => ModelStepResult.IllegalSelector
      end match
    end index

    override def key(base: Model, key: String): ModelStepResult[Model] =
      base match
        case Model.Object(items) =>
          items.get(key) match
            case None => ModelStepResult.MissingValue
            case Some(x) => ModelStepResult.Success(x)
          end match
        case _ => ModelStepResult.IllegalSelector
      end match
    end key
  end given

end QuerySyntaxTest
