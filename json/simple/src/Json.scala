package io.github.maxkar
package json.simple

import fun.typeclass.Monad
import fun.instances.Unnest

import json.writer.DefaultWriter
import json.writer.Layout
import json.writer.Literals
import json.writer.Strings
import json.writer.Objects
import json.writer.Arrays

import json.parser.Peek
import json.parser.DefaultStream
import json.parser.ParseError
import java.io.StringWriter


/** Instruction on how to update specific object element. */
abstract sealed class ObjectUpdateInstruction

/** An "optional" value that may be present or absent. */
abstract sealed class MaybeJson extends ObjectUpdateInstruction

/**
 * Single node in the JSON tree model.
 */
abstract sealed class Json extends MaybeJson {
  /** Outputs this JSON into the given stream in the compact form. */
  inline def writeCompact[M[_]: Monad, S: DefaultWriter.In[M]](stream: S): M[Unit] =
    Json.writeCompact(stream, this)


  /** Outputs this JSON into the given stream in the pretty form. */
  inline def writePretty[M[_]: Monad, S: DefaultWriter.In[M]](
        stream: S,
        indent: Int = 2,
      ): M[Unit] =
    Json.writePretty(stream, this, indent)

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
        indent: Int = 2,
      ): java.lang.String =
    Json.toPrettyString(this, indent)
}


object Json {
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
  case class Array(elements: Seq[Json]) extends Json {
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
  }

  /** Json Object. */
  case class Object(elements: Map[java.lang.String, Json]) extends Json {
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
    def update(instructions: (java.lang.String, ObjectUpdateInstruction)*): Object = {
      var res = elements
      val itr = instructions.iterator

      while itr.hasNext do {
        val (key, instruction) = itr.next()
        instruction match {
          case Remove => res = res - key
          case Empty => ()
          case other: Json => res = res + ((key -> other))
        }
      }
      Object(res)
    }
  }


  /**
   * Retruns simple name of the json type (i.e. boolean, number, etc...).
   */
  def typeName(v: Json): java.lang.String =
    v match {
      case Json.Null => "null"
      case Json.True | Json.False => "boolean"
      case Json.Number(_) => "number"
      case Json.String(_) => "string"
      case Json.Object(_) => "object"
      case Json.Array(_) => "array"
    }


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


  /** Reads a single value from the stream and stops after the value was read. */
  def readOneValue[M[_]: Monad, S: Peek.In[M]: DefaultStream.In[M]: ParseError.In[M]](stream: S): M[Json] =
    new Reader().readValue(stream)


  /**
   * Reads value from the stream ensuring that no other data is contained in
   * the `stream`. In other words, it reads the **whole** stream as a single
   * JSON value.
   */
  def read[M[_]: Monad, S: Peek.In[M]: DefaultStream.In[M]: ParseError.In[M]](stream: S): M[Json] =
    new Reader().readFully(stream)


  /** Outputs JSON into the given stream in the compact form. */
  def writeCompact[M[_]: Monad, S: DefaultWriter.In[M]](stream: S, v: Json): M[Unit] =
    write(stream, v, new Layout.Compact)


  /** Outputs JSON into the given stream in the pretty form. */
  def writePretty[M[_]: Monad, S: DefaultWriter.In[M]](
        stream: S,
        v: Json,
        indent: Int = 2,
      ): M[Unit] =
    write(stream, v, Layout.Indent(indent))


  /** Outputs JSON into the stream in the given format. */
  def write[M[_]: Monad, S: DefaultWriter.In[M]](
        stream: S,
        value: Json,
        format: Layout[M, S],
      ): M[Unit] =
    value match {
      case Json.Null =>
        stream.write(Literals.NULL)
      case Json.True =>
        stream.write(Literals.TRUE)
      case Json.False =>
        stream.write(Literals.FALSE)
      case Json.String(value) =>
        Strings.write(stream, value)
      case Json.Number(value) =>
        stream.write(value)
      case Json.Array(elements) =>
        Arrays.writeAll(stream, format.arrayLayout, write[M, S](_, _, format.nested), elements)
      case Json.Object(elements) =>
        Objects.writeAll[M, S, (java.lang.String, Json)](
          stream,
          format.objectLayout,
          { (stream, entry) => Strings.write(stream, entry._1) },
          { (stream, entry) => write(stream, entry._2, format.nested)},
          elements
        )
    }

  /** Returns compact string representation of the given JSON. */
  def toCompactString(v: Json): java.lang.String = {
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringWriter()
    Unnest.run(writeCompact(stream, v))
    stream.toString()
  }


  /** Returns pretty string representation of the given JSON. */
  def toPrettyString(
        v: Json,
        indent: Int = 2,
      ): java.lang.String = {
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringWriter()
    Unnest.run(writePretty(stream, v, indent))
    stream.toString()
  }
}
