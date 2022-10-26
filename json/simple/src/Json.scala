package io.github.maxkar
package json.simple

import fun.typeclass.Monad
import fun.instances.Unnest

import text.input.LookAheadStream
import text.output.{Stream => OutStream}
import text.output.StringBuilderStream

import json.parser.{Values => JsonReader}
import json.parser.Values.AllErrors
import json.parser.EndOfFile
import json.writer.{Values => JsonWriter}
import json.writer.PrettyPrintOptions
import json.writer.Values.ValueClassifier
import json.writer.Values.ValueCallback


/** Instruction on how to update specific object element. */
abstract sealed class ObjectUpdateInstruction

/** An "optional" value that may be present or absent. */
abstract sealed class MaybeJson extends ObjectUpdateInstruction

/**
 * Single node in the JSON tree model.
 */
abstract sealed class Json extends MaybeJson:
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
  /** Instruction to remove JSON element with the given name. */
  case object Remove extends ObjectUpdateInstruction

  /** "No value" in the context that accepts optional JSON values. */
  case object Empty extends MaybeJson

  /** Json `null` literal. */
  case object Null extends Json

  /** True literal. */
  case object True extends Json

  /** False literal. */
  case object False extends Json

  /** Json String value. */
  case class String(value: java.lang.String) extends Json

  /** Json number (with the given numeric representation). */
  case class Number(repr: java.lang.String) extends Json

  /** Json Array. */
  case class Array(elements: Seq[Json]) extends Json:
    /** Creates an array that is concatenation of this array and another array. */
    def ++(another: Array): Array =
      Array(elements ++ another.elements)

    /** Creates an array that is concatenation of this array and `items`. */
    def concat(items: Seq[Json]): Array =
      Array(elements ++ items)

    /**
     * Updates the array with the new contents. This is mostly similar to concat
     * but takes varargs and supports optional elements:
     * ```
     * val arr: Json.Array = generateSomeArray()
     * val newArr =
     *   arr.update(
     *     1,
     *     2,
     *     createOptElement(),
     *     3,
     *   )
     *
     *
     * def createOptElement(): MaybeJson = Json.Empty
     * ```
     *
     * This way values 1, 2 and 3 will be appended. Empty elements are skipped.
     */
    def update(items: MaybeJson*): Array =
      Array(
        elements ++ (items.collect { case x: Json => x})
      )
  end Array

  /** Json Object. */
  case class Object(elements: Map[java.lang.String, Json]) extends Json:
    /**
     * Creates an array that is "concatenation" of this object and another object.
     * Values from `another` object will take precedence over values present in this object.
     */
    def ++(another: Object): Object =
      Object(elements ++ another.elements)


    /**
     * Creates an object that is concatenation of this object and `items`.
     * Elements in the `items` takes precedence over elements in this object.
     */
    def concat(items: Map[java.lang.String, Json]): Object =
      Object(elements ++ items)


    /**
     * Creates an object that is concatenation of this object and `items`.
     * Elements in the `items` takes precedence over elements in this object.
     */
    def concatSeq(items: Seq[(java.lang.String, Json)]): Object =
      Object(elements ++ items)


    /**
     * Updates the object according to the given instructions. The instructions are
     * treated as follows:
     *  * Regular JSON value updates (or creates) value of the given key.
     *  * The `Json.Empty` value does not update anything. The instruction is still useful
     *    when update is generated automatically from an "optional" update value.
     *  * The `Json.Remove` value removes the key from the resulting object.
     *
     * Instructions are processed in the order provided.
     */
    def update(instructions: (java.lang.String, ObjectUpdateInstruction)*): Object =
      var res = elements
      val itr = instructions.iterator

      while itr.hasNext do
        val (key, instruction) = itr.next()
        instruction match
          case Remove => res = res - key
          case Empty => ()
          case other: Json => res = res + ((key -> other))
        end match
      end while
      Object(res)
    end update
  end Object

  /**
   * An implementation of the JSON builder from the given dispatch over the model.
   */
  private final class FromBuilder[T](
        dispatch: ValueClassifier[T],
      )
      extends ValueCallback[T, Unnest[Json]]:
    import Unnest.given

    /** Converts value to json. */
    def convert(x: T): Unnest[Json] = dispatch.classifyValue(x, this)

    override def nullValue(): Unnest[Json] =
      Monad.pure(Json.Null)

    override def boolean(v: Boolean): Unnest[Json] =
      Monad.pure(if (v) then Json.True else Json.False)

    override def number(representation: CharSequence): Unnest[Json] =
      Monad.pure(Json.Number(representation.toString()))

    override def string(v: CharSequence): Unnest[Json] =
      Monad.pure(Json.String(v.toString()))


    override def array(iter: Iterator[T]): Unnest[Json] =
      arrayAgg(iter, List.empty)


    override def unorderedObject(iter: Iterator[(java.lang.String, T)]): Unnest[Json] =
      objectAgg(iter, List.empty)


    /** Aggregates array in the monadic way. */
    private def arrayAgg(iter: Iterator[T], agg: List[Json]): Unnest[Json] =
      if iter.hasNext then
        convert(iter.next()).flatMap { x => arrayAgg(iter, x :: agg) }
      else
        Monad.pure(Json.Array(agg.reverseIterator.toSeq))
    end arrayAgg


    /** Aggregates object in the monadic way. */
    private def objectAgg(
          iter: Iterator[(java.lang.String, T)],
          agg: List[(java.lang.String, Json)]
        ):  Unnest[Json] =
      if iter.hasNext then
        val (k, v) = iter.next()
        convert(v).flatMap { x => objectAgg(iter, (k -> x) :: agg) }
      else
        Monad.pure(Json.Object(agg.toMap))
    end objectAgg
  end FromBuilder


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


  /**
   * Constructs JSON array from individual elements.
   *
   * Usage:
   * ```
   * val x: Json = Json.array(1, 2, 3)
   * ```
   */
  def array(elements: MaybeJson*): Json.Array =
    Json.Array(
      elements.collect { case x: Json => x }
    )


  /**
   * Creates a new object from json elements.
   * Usage:
   * ```
   * val x: Json =
   *   Json.make(
   *     "a" -> 25,
   *     "b" -> "test"
   *   )
   * ```
   */
  def make(entries: (java.lang.String, MaybeJson)*): Json.Object =
    makeFrom(entries.iterator)


  /** Creates a new object from json elements provided by the iterator. */
  def makeFrom(entries: Iterator[(java.lang.String, MaybeJson)]): Json.Object =
    Json.Object(
      entries
        .collect { case (key, value: Json) => (key -> value) }
        .toMap
    )

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


  /**
   * Creates an instance of JSON from another (writeable) model.
   * @tparam T another JSON model.
   * @param other another instance to create the data from.
   * @return JSON representation of the `other` value.
   */
  def from[T](other: T)(implicit format: ValueClassifier[T]): Json =
    Unnest.run(new FromBuilder(format).convert(other))


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
