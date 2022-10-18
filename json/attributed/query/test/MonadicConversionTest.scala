package io.github.maxkar
package json.attr.query

import json.query.Query
import json.query.Path
import json.attr.Json
import json.attr.factory.Factory

import json.parser.JsonParser
import json.parser.chunky.Module
import json.parser.chunky.SourceLocation

import json.attr.query.given
import defaultConversions.given

import fun.typeclass.Monad
import fun.typeclass.Collect

import scala.language.implicitConversions

/**
 * A test for monadic (higher-kinded) json parsing and conversion. Illustrates some
 * "real-world" scenario and may be used as an implementation example.
 */
final class MonadicConversionTest extends org.scalatest.funsuite.AnyFunSuite:
  import MonadicConversionTest.Attrs
  import MonadicConversionTest.parse

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
  given convertibleImpl: ConvertibleBy[Md, Attrs] with
    override def pure[T](v: T): Md[T] = Right(v)

    /** Generates locatios string for a source location. */
    private def sloc(loc: SourceLocation): String =
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
    Inner.apply ≻≻ q


  /** Parse outer dto in a cool way. */
  def parseDto(q: Query[Json[Attrs]]): Md[Dto] =
    Dto.apply.curried ≻≻
      q.x ≻
      q.y ≻
      q.z ≻
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
end MonadicConversionTest


object MonadicConversionTest:
  /** Parsing module & operations. */
  private val module = Module[String](unexpectedEof = loc => s"${loc}: Unexpected EOF")
  import module.given

  /** Attributes of the resulting json. */
  type Attrs = (SourceLocation, SourceLocation)


  /** Factory of the json values. */
  private val factory =
    new Factory[module.Parser, SourceLocation, Attrs](
      location = module.location,
      locationToString = l => s"${l.line}:${l.column} (offset=${l.offset})",
      fail = msg => module.abort(msg),
      makeAttrs = (start, end) => (start, end)
    )

  /** Parser to use in tests. */
  private val parser = new JsonParser(parserInput, factory)

  /** Runs the parser on the given input with with the given chunk size. */
  private def runParser(input: String): Either[String, Json[Attrs]] =
    val consumer = module.start(parser.parseValue)

    var pos = 0
    while pos < input.length do
      val nextChunkSize = input.length - pos
      val consumed = consumer.process(input.subSequence(pos, pos + nextChunkSize))
      /* Consumer indicated that it does not want any more input. */
      if consumed < nextChunkSize then
        return consumer.end()

      pos += consumed
    end while

    consumer.end()
  end runParser


  private def parse(input: String): Json[Attrs] =
    runParser(input) match
      case Right(x) => x
      case Left(err) => throw new java.io.IOException(s"Errors parsing json: ${err}")
    end match
  end parse
end MonadicConversionTest
