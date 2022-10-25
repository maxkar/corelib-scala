package io.github.maxkar
package json.simple

import fun.typeclass.Monad

import text.input.LookAheadStream
import text.output.{Stream => OutStream}
import text.output.StringBuilderStream

import json.parser.{Values => JsonReader}
import json.parser.Values.AllErrors
import json.parser.EndOfFile
import json.writer.{Values => JsonWriter}
import json.writer.PrettyPrintOptions


/**
 * Single node in the JSON tree model.
 */
enum Json:
  /** Json `null` literal. */
  case Null
  /** True literal. */
  case True
  /** False literal. */
  case False
  /** Json String value. */
  case String(value: java.lang.String)
  /** Json number (with the given numeric representation). */
  case Number(repr: java.lang.String)
  /** Json Array. */
  case Array(elements: Seq[Json])
  /** Json Object. */
  case Object(elements: Map[java.lang.String, Json])


  /** Outputs this JSON into the given stream in the compact form. */
  inline def writeCompact[M[_]: Monad](stream: OutStream[M]): M[Unit] =
    Json.writeCompact(this, stream)


  /** Outputs this JSON into the given stream in the pretty form. */
  inline def writePretty[M[_]: Monad](
        stream: OutStream[M],
        format: PrettyPrintOptions = PrettyPrintOptions.defaultOptions,
      ): M[Unit] =
    Json.writePretty(this, stream, format)

  /**
   * Returns pretty string representation of this value.
   * This function is recursive and may cause stack overflow on large objects.
   */
  inline def toCompactString(): java.lang.String =
    Json.toCompactString(this)


  /**
   * Returns pretty string representation of this value.
   * This function is recursive and may cause stack overflow on large objects.
   */
  inline def toPrettyString(
        format: PrettyPrintOptions = PrettyPrintOptions.defaultOptions,
      ): java.lang.String =
    Json.toPrettyString(this, format)
end Json


object Json:
  /**
   * Retruns simple name of the json type (i.e. boolean, number, etc...).
   */
  def typeName(v: Json): java.lang.String =
    v match
      case Json.Null => "null"
      case Json.True | Json.False => "boolean"
      case Json.Number(_) => "number"
      case Json.String(_) => "string"
      case Json.Object(_) => "object"
      case Json.Array(_) => "array"
    end match
  end typeName


  /** Reads a simple value from the stream and stops after the value was read. */
  def readOneValue[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S,
      )(implicit
        errors: AllErrors[M, S],
      ): M[Json] =
    JsonReader.readSimple(Builder, stream)
  end readOneValue


  /**
   * Reads value from the stream ensuring that no other data is contained in
   * the `stream`. In other words, it reads the **whole** stream as a single
   * JSON value.
   */
  def read[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S
      )(implicit
        errors: AllErrors[M, S],
        eofErrors: EndOfFile.Errors[M, S]
      ): M[Json] =
    for {
      res <- readOneValue(stream)
      _ <- EndOfFile.expectNoValues(stream)
    } yield res
  end read


  /** Outputs JSON into the given stream in the compact form. */
  def writeCompact[M[_]: Monad](v: Json, stream: OutStream[M]): M[Unit] =
    JsonWriter.writeCompact(v, stream)


  /** Outputs JSON into the given stream in the pretty form. */
  def writePretty[M[_]: Monad](
        v: Json,
        stream: OutStream[M],
        format: PrettyPrintOptions = PrettyPrintOptions.defaultOptions,
      ): M[Unit] =
    JsonWriter.writePretty(format, v, stream)


  /** Returns compact string representation of the given JSON. */
  def toCompactString(v: Json): java.lang.String =
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringBuilderStream()
    Unnest.run(writeCompact(v, stream))
    stream.data
  end toCompactString


  /** Returns pretty string representation of the given JSON. */
  def toPrettyString(
        v: Json,
        format: PrettyPrintOptions = PrettyPrintOptions.defaultOptions,
      ): java.lang.String =
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringBuilderStream()
    Unnest.run(writePretty(v, stream, format))
    stream.data
  end toPrettyString
end Json
