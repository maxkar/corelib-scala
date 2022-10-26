package io.github.maxkar
package json.simple

import json.query.Query
import json.query.Path
import json.simple.Json

import json.simple.given
import defaultConversions.given

import fun.typeclass.Monad
import fun.typeclass.Collect

import scala.language.implicitConversions

/**
 * A test for monadic (higher-kinded) json parsing and conversion. Illustrates some
 * "real-world" scenario and may be used as an implementation example.
 */
final class MonadicConversionTest extends org.scalatest.funsuite.AnyFunSuite:
  /**
   * The monad we use for parsing. Our particular implementation collects all
   * errors encountered during parsing into the Left part.
   */
  type Md[T] = Either[Seq[String], T]


  /** Monad implementation. */
  given mdImpl: Monad[Md] with
    override def pure[T](v: T): Md[T] = Right(v)

    override def bind[S, R](v: Md[S], fn: S => Md[R]): Md[R] =
      v match
        case Left(x) => Left(x)
        case Right(v) => fn(v)
      end match
    end bind

    /* We need custom applicative here - we want to preserve both sides of error. */
    override def aapply[S, R](v: Md[S], fn: Md[S => R]): Md[R] =
      (v, fn) match {
        case (Left(x), Left(y)) => Left(y ++ x)
        case (Left(x), _) => Left(x)
        case (_, Left(y)) => Left(y)
        case (Right(xv), Right(fv)) => Right(fv(xv))
      }
  end mdImpl


  /** How to collect multiple operations into one. */
  given collectImpl: Collect[Md] with
    override def collect[T](items: Seq[Md[T]]): Md[Seq[T]] =
      val (errors, successes) = items.partitionMap(identity)
      if errors.nonEmpty then
        Left(errors.flatten)
      else
        Right(successes)
    end collect
  end collectImpl


  /** How to encode errors. */
  given convertibleImpl: ConvertibleBy[Md] with
    override def pure[T](v: T): Md[T] = Right(v)

    override def accessError[T](validPath: Path, value: Json, invalidPath: Path): Md[T] =
      Left(Seq(s"${validPath}:AcessError[path=${invalidPath}]"))

    override def fieldMissing[T](validPath: Path, value: Json, invalidPath: Path): Md[T] =
      Left(Seq(s"${validPath}:FieldMissing[path=${invalidPath}]"))

    override def invalidDomainValue[T](path: Path, value: Json, message: String): Md[T] =
      Left(Seq(s"${path}:InvalidValue[message=${message}]"))
  end convertibleImpl


  /** Inner DTO. */
  case class Inner(v: Seq[Int])

  /** Dto we would like to parse in the example. */
  case class Dto(x: Int, y: String, z: Boolean, inner: Inner)


  /** Parse inner object in a cool way. */
  def parseInner(q: Query[Json]): Md[Inner] =
    Inner.apply ≻≻ q


  /** Parse outer dto in a cool way. */
  def parseDto(q: Query[Json]): Md[Dto] =
    Dto.apply.curried ≻≻
      q.x ≻
      q.y ≻
      q.z ≻
      parseInner(q.inner)


  test("Valid object is parsed") {
    val model =
      Json.Object(Map(
        "x" -> Json.Number("45"),
        "y" -> Json.String("Hello, Monad"),
        "z" -> Json.False,
        "inner" -> Json.Array(Seq(Json.Number("1"), Json.Number("2"), Json.Number("3")))
      ))

    val maybeDto = parseDto(Query(model))
    assert(maybeDto === Right(Dto(45, "Hello, Monad", false, Inner(Seq(1, 2, 3)))))
  }


  test("Invalid object generates expected messages") {
    val model =
      Json.Object(Map(
        "x" -> Json.String("Hello, Monad"),
        "z" -> Json.Null,
        "inner" -> Json.Array(Seq(Json.Number("1.5"), Json.Number("2"), Json.Number("2.5")))
      ))

    val maybeDto = parseDto(Query(model))
    val errors = Seq(
      "x:InvalidValue[message=Can't convert string to int]",
      "<root>:FieldMissing[path=y]",
      "z:InvalidValue[message=Can't convert null to boolean]",
      "inner[0]:InvalidValue[message=Can't convert 1.5 to int]",
      "inner[2]:InvalidValue[message=Can't convert 2.5 to int]",
    )
    assert(maybeDto === Left(errors))
  }


  test("Parsing plays nicely with stupid syntax") {
    val model =
      Json.Object(Map(
        "x" -> Json.Number("45"),
        "y" -> Json.String("Hello, Monad"),
        "z" -> Json.False,
        "inner" -> Json.Array(Seq(Json.Number("1"), Json.Number("2"), Json.Number("3")))
      ))

    val stupid =
      Json.Object(Map(
        "map" -> Json.Array(Seq(
          model
        ))
      ))

    val expectedDto = Right(Dto(45, "Hello, Monad", false, Inner(Seq(1, 2, 3))))

    val maybeDto1 = parseDto(Query(stupid)("map", 0))
    assert(maybeDto1 === expectedDto)

    val maybeDto2 = parseDto(Query(stupid) / "map" / 0)
    assert(maybeDto2 === expectedDto)


    val lessStupid =
      Json.Object(Map(
        "data" -> Json.Array(Seq(
          model
        ))
      ))

    val maybeDto3 = parseDto(Query(lessStupid).data(0))
    assert(maybeDto3 === expectedDto)
  }

end MonadicConversionTest
