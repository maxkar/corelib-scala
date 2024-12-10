package io.github.maxkar
package json.attr

import fun.typeclass.Monad

import text.input.LookAheadStream
import text.output.{Stream => OutStream}
import text.output.StringBuilderStream

import json.parser.Values
import json.parser.EndOfFile

import json.writer.{Values => JsonWriter}
import json.writer.PrettyPrintOptions

/**
 * Single node in the JSON tree model.
 * @tparam A type of the attributes that are applied to json.
 */
abstract sealed class Json[+A] {
  /** Attributes of this JSON node. */
  val attrs: A

  /**
   * Builds a new json by applying function to attributes of this json
   * element and attributes of nested elemnts (if any). The calculation
   * is eager.
   * @param fn function to apply to json attributes.
   */
  def mapAttributes[R](fn: A => R): Json[R]


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
}


object Json {
  /**
   * Information about one entry in the json object. It captures information
   * related to the object's key along with the value associated with the key.
   *
   * @param key key of the entry.
   * @param keyAttrs attributes applicable to the entry's key.
   * @param value value of the given entry.
   */
  case class ObjectEntry[+A](key: java.lang.String, keyAttrs: A, value: Json[A]) {
    /**
     * Builds a new json entry by applying function to both key's and value's attributes.
     * @param fn function to apply to json attributes.
     */
    def mapAttributes[R](fn: A => R): ObjectEntry[R] =
      ObjectEntry(key, fn(keyAttrs), value.mapAttributes(fn))
  }


  /** Representation of the json "null" literal. */
  case class Null[+A](attrs: A) extends Json[A] {
    override def mapAttributes[R](fn: A => R): Json[R] =
      Null(fn(attrs))
  }

  /** Representation of the json "true" literal. */
  case class True[+A](attrs: A) extends Json[A] {
    override def mapAttributes[R](fn: A => R): Json[R] =
      True(fn(attrs))
  }

  /** Representation of the json "false" literal. */
  case class False[+A](attrs: A) extends Json[A] {
    override def mapAttributes[R](fn: A => R): Json[R] =
      True(fn(attrs))
  }

  /**
   * JSON string literal.
   * @param value value of the literal.
   */
  case class String[+A](value: java.lang.String, attrs: A) extends Json[A] {
    override def mapAttributes[R](fn: A => R): Json[R] =
      String(value, fn(attrs))
  }

  /**
   * String representation of the JSON numeric literal. The node contains
   * "raw" (unparesd) value but ensures it is well-formed number according
   * to the JSON.
   * @param value raw (unparsed) value.
   */
  case class Number[+A](value: java.lang.String, attrs: A) extends Json[A] {
    override def mapAttributes[R](fn: A => R): Json[R] =
      String(value, fn(attrs))
  }


  /**
   * Json array.
   * @param elements elemenst of the JSON array.
   * @param attrs attributes applicable to this array.
   */
  case class Array[+A](elements: Seq[Json[A]], attrs: A) extends Json[A] {
    /** Retrieves nth element of the array. */
    def apply(index: Int): Json[A] = elements(index)

    override def mapAttributes[R](fn: A => R): Json[R] =
      Array(elements.map(_.mapAttributes(fn)), fn(attrs))
  }


  /**
   * Json object.
   * @param elements entries of the object (object keys must match map entry keys).
   * @param attrs attributes of this json value (complete object).
   */
  case class Object[+A](elements: Map[java.lang.String, ObjectEntry[A]], attrs: A) extends Json[A] {
    /** Retrieves value associated with the given key. */
    def apply(key: java.lang.String): Json[A] = elements(key).value

    /** Retrieves optional value associated with the key. */
    def get(key: java.lang.String): Option[Json[A]] = elements.get(key).map(_.value)

    override def mapAttributes[R](fn: A => R): Json[R] =
      Object(elements.view.mapValues(_.mapAttributes(fn)).toMap, fn(attrs))
  }


  /**
   * Returns simple name of the JSON type.
   */
  def typeName(v: Json[?]): java.lang.String =
    v match {
      case Json.Null(_) => "null"
      case Json.True(_) | Json.False(_) => "boolean"
      case Json.String(_, _) => "string"
      case Json.Number(_, _) => "number"
      case Json.Array(_, _) => "array"
      case Json.Object(_, _) => "object"
    }


  /**
   * Reads a single value from the stream and stops after the value was read.
   *
   * @param stream data stream to read.
   * @param attributeFactory factory used to create JSON attributes from data
   *   available through the given stream.
   */
  inline def readOneValue[M[_]: Monad, S <: LookAheadStream[M], A](
        stream: S,
        attributeFactory: AttributeFactory[M, S, A]
      )(using
        errs: Values.AllErrors[M, S],
        attrErrors: Reader.Errors[M, S, A]
      ): M[Json[A]] =
    Reader.readOneValue(stream, attributeFactory)

  /**
   * Reads value from the stream ensuring that no other data is contained in
   * the `stream`. In other words, it reads the **whole** stream as a single
   * JSON value.
   *
   * @param stream data stream to read.
   * @param attributeFactory factory used to create JSON attributes from data
   *   available through the given stream.
   */
  inline def read[M[_]: Monad, S <: LookAheadStream[M], A](
        stream: S,
        attributeFactory: AttributeFactory[M, S, A]
      )(using
        errs: Values.AllErrors[M, S],
        attrErrors: Reader.Errors[M, S, A],
        eofErrors: EndOfFile.Errors[M, S],
      ): M[Json[A]] =
    Reader.read(stream, attributeFactory)


  /** Outputs JSON into the given stream in the compact form. */
  inline def writeCompact[M[_]: Monad, A](v: Json[A], stream: OutStream[M]): M[Unit] =
    JsonWriter.writeCompact(v, stream)


  /** Outputs JSON into the given stream in the pretty form. */
  inline def writePretty[M[_]: Monad, A](
        v: Json[A],
        stream: OutStream[M],
        format: PrettyPrintOptions = PrettyPrintOptions.defaultOptions,
      ): M[Unit] =
    JsonWriter.writePretty(format, v, stream)


  /** Returns compact string representation of the given JSON. */
  def toCompactString(v: Json[?]): java.lang.String = {
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringBuilderStream()
    Unnest.run(writeCompact(v, stream))
    stream.data
  }


  /** Returns pretty string representation of the given JSON. */
  def toPrettyString(
        v: Json[?],
        format: PrettyPrintOptions = PrettyPrintOptions.defaultOptions,
      ): java.lang.String = {
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringBuilderStream()
    Unnest.run(writePretty(v, stream, format))
    stream.data
  }
}
