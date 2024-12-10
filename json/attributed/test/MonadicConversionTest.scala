package io.github.maxkar
package json.attr

import java.nio.CharBuffer

import text.Location
import text.input.LocationLookAheadStream
import text.input.BufferLookAheadStream

import json.query.Query
import json.query.Path
import json.attr.Json
import json.attr.Reader
import json.attr.AttributeFactory

import json.parser.Errors

import defaultConversions.given

import fun.typeclass.Monad
import fun.typeclass.Collect

import scala.language.implicitConversions

/**
 * A test for monadic (higher-kinded) json parsing and conversion. Illustrates some
 * "real-world" scenario and may be used as an implementation example.
 */
final class MonadicConversionTest extends org.scalatest.funsuite.AnyFunSuite {
  import MonadicConversionTest._
  import MonadicConversionTest.given


  /** How to encode errors. */
  given convertibleImpl: ConvertibleBy[Md, Attrs] with
    override def pure[T](v: T): Md[T] = Right(v)

    /** Generates locatios string for a source location. */
    private def sloc(loc: Location): String =
      s"(${loc.line}:${loc.column})"

    /** Generates location string from attributes. */
    private def loc(attrs: Attrs, path: Path): String =
      s"${sloc(attrs._1)}-${sloc(attrs._2)} (path=${path})"

    override def accessError[T](validPath: Path, value: Json[Attrs], invalidPath: Path): Md[T] =
      Left(Seq(s"${loc(value.attrs, validPath)}:AcessError[path=${invalidPath}]"))

    override def fieldMissing[T](validPath: Path, value: Json[Attrs], invalidPath: Path): Md[T] =
      Left(Seq(s"${loc(value.attrs, validPath)}:FieldMissing[path=${invalidPath}]"))

    override def invalidDomainValue[T](path: Path, value: Json[Attrs], message: String): Md[T] =
      Left(Seq(s"${loc(value.attrs, path)}:InvalidValue[message=${message}]"))
  end convertibleImpl


  /** Inner DTO. */
  case class Inner(v: Seq[Int])

  /** Dto we would like to parse in the example. */
  case class Dto(x: Int, y: String, z: Boolean, inner: Inner)


  /** Parse inner object in a cool way. */
  def parseInner(q: Query[Json[Attrs]]): Md[Inner] =
    Inner.apply ||> q


  /** Parse outer dto in a cool way. */
  def parseDto(q: Query[Json[Attrs]]): Md[Dto] =
    Dto.apply.curried ||>
      q.x |>
      q.y |>
      q.z |>
      parseInner(q.inner)


  test("Valid object is parsed") {
    val model = parse(
      """{
        |  "x": 45,
        |  "y": "Hello, Monad",
        |  "z": false,
        |  "inner": [1, 2, 3]
        |}""".stripMargin
    )

    val maybeDto = parseDto(Query(model))
    assert(maybeDto === Right(Dto(45, "Hello, Monad", false, Inner(Seq(1, 2, 3)))))
  }


  test("Invalid object generates expected messages") {
    val model = parse(
      """{
        |  "x": "Hello, Monad",
        |  "z": null,
        |  "inner": [1.5, 2, 2.5]
        |}""".stripMargin
    )

    val maybeDto = parseDto(Query(model))
    val errors = Seq(
      "(2:8)-(2:22) (path=x):InvalidValue[message=Can't convert string to int]",
      "(1:1)-(5:2) (path=<root>):FieldMissing[path=y]",
      "(3:8)-(3:12) (path=z):InvalidValue[message=Can't convert null to boolean]",
      "(4:13)-(4:16) (path=inner[0]):InvalidValue[message=Can't convert 1.5 to int]",
      "(4:21)-(4:24) (path=inner[2]):InvalidValue[message=Can't convert 2.5 to int]",
    )
    assert(maybeDto === Left(errors))
  }


  test("Parsing plays nicely with stupid syntax") {
    val stupid = parse(
      """{
        |  "map": [
        |    {
        |      "x": 45,
        |      "y": "Hello, Monad",
        |      "z": false,
        |      "inner": [1, 2, 3]
        |    }
        |  ]
        |}""".stripMargin
    )

    val expectedDto = Right(Dto(45, "Hello, Monad", false, Inner(Seq(1, 2, 3))))

    val maybeDto1 = parseDto(Query(stupid)("map", 0))
    assert(maybeDto1 === expectedDto)

    val maybeDto2 = parseDto(Query(stupid) / "map" / 0)
    assert(maybeDto2 === expectedDto)


    val lessStupid = parse(
      """{
        |  "data": [
        |    {
        |      "x": 45,
        |      "y": "Hello, Monad",
        |      "z": false,
        |      "inner": [1, 2, 3]
        |    }
        |  ]
        |}""".stripMargin
    )

    val maybeDto3 = parseDto(Query(lessStupid).data(0))
    assert(maybeDto3 === expectedDto)
  }
}


object MonadicConversionTest {
  /** Attributes of the resulting json. */
  type Attrs = (Location, Location)

  /**
   * The monad we use for parsing. Our particular implementation collects all
   * errors encountered during parsing into the Left part.
   */
  type Md[T] = Either[Seq[String], T]

  /** Monad implementation. */
  given mdImpl: Monad[Md] with {
    override def pure[T](v: T): Md[T] = Right(v)

    override def bind[S, R](v: Md[S], fn: S => Md[R]): Md[R] =
      v match {
        case Left(x) => Left(x)
        case Right(v) => fn(v)
      }

    /* We need custom applicative here - we want to preserve both sides of error. */
    override def aapply[S, R](v: Md[S], fn: Md[S => R]): Md[R] =
      (v, fn) match {
        case (Left(x), Left(y)) => Left(y ++ x)
        case (Left(x), _) => Left(x)
        case (_, Left(y)) => Left(y)
        case (Right(xv), Right(fv)) => Right(fv(xv))
      }
  }


  /** How to collect multiple operations into one. */
  given collectImpl: Collect[Md] with {
    override def collect[T](items: Seq[Md[T]]): Md[Seq[T]] = {
      val (errors, successes) = items.partitionMap(identity)
      if errors.nonEmpty then
        Left(errors.flatten)
      else
        Right(successes)
    }
  }


  /** Factory for the attributes. */
  private val attrFactory = AttributeFactory.span

  /** Simple implementation of the error handler. */
  private object RaiseError extends Errors.SimpleHandler[Md, LocationLookAheadStream[Md, Any]] {
    override def raise[T](stream: LocationLookAheadStream[Md, Any], message: String): Md[T] =
      Left(Seq(s"${stream.location}: ${message}"))
  }


  /** Error handler for all the errors. */
  given errorHandler: Errors.ErrorHandler[Md, LocationLookAheadStream[Md, Any]] =
    Errors.simple[Md, LocationLookAheadStream[Md, Any]](RaiseError)
  import errorHandler.endOfFileErrors


  /** Attribute-specific errors. */
  given attrErrors: Reader.Errors[Md, LocationLookAheadStream[Md, Any], Attrs] with {
    override def duplicateObjectKey(
          prevEntry: Json.ObjectEntry[Attrs],
          newKeyAttrs: Attrs,
          stream: LocationLookAheadStream[Md, Any],
        ): Md[Unit] =
      Left(Seq(
        s"${newKeyAttrs._1}: Duplicate key ${prevEntry.key}, previous definition at ${prevEntry.keyAttrs._1}"
      ))
  }


  /** Runs the parser on the given input with with the given chunk size. */
  private def runParser(input: String): Md[Json[Attrs]] = {
    val reader = new java.io.StringReader(input)
    val filler = BufferLookAheadStream.Filler[Md](reader, Right(()), x => Left(Seq(x.toString)))
    val baseInputStream = BufferLookAheadStream(filler, CharBuffer.allocate(10))
    val inputStream = LocationLookAheadStream(baseInputStream)
    Reader.read(inputStream, attrFactory)
  }


  private def parse(input: String): Json[Attrs] =
    runParser(input) match {
      case Right(x) => x
      case Left(err) => throw new java.io.IOException(s"Errors parsing json: ${err}")
    }
}
