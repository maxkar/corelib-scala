package io.github.maxkar
package json.attr

import fun.typeclass.Monad

import text.output.{Stream => OutStream}
import text.output.StringBuilderStream

import json.parser.Peek
import json.parser.DefaultStream
import json.writer.DefaultWriter
import json.writer.Layout
import json.writer.Literals
import json.writer.Strings
import json.writer.Objects
import json.writer.Arrays
import java.io.StringWriter


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


  /** Outputs JSON into the given stream in the compact form. */
  def writeCompact[M[_]: Monad, S: DefaultWriter.In[M], A](
        stream: S,
        value: Json[A],
        objectKeyOrder: Iterable[ObjectEntry[A]] => Iterable[ObjectEntry[A]] = identity,
      ): M[Unit] =
    write(stream, value, new Layout.Compact(), objectKeyOrder)


  /** Outputs JSON into the given stream in the pretty form. */
  inline def writePretty[M[_]: Monad, S: DefaultWriter.In[M], A](
        stream: S,
        value: Json[A],
        indent: Int = 2,
        objectKeyOrder: Iterable[ObjectEntry[A]] => Iterable[ObjectEntry[A]] = identity,
      ): M[Unit] =
    write(stream, value, Layout.Indent(2), objectKeyOrder)


  /** Outputs JSON into the stream in the given format. */
  def write[M[_]: Monad, S: DefaultWriter.In[M], A](
        stream: S,
        value: Json[A],
        format: Layout[M, S],
        objectKeyOrder: (Iterable[ObjectEntry[A]] => Iterable[ObjectEntry[A]]) = identity,
      ): M[Unit] =
    value match {
      case Json.Null(_) =>
        stream.write(Literals.NULL)
      case Json.True(_) =>
        stream.write(Literals.TRUE)
      case Json.False(_) =>
        stream.write(Literals.FALSE)
      case Json.String(value, _) =>
        Strings.write(stream, value)
      case Json.Number(value, _) =>
        stream.write(value)
      case Json.Array(elements, _) =>
        Arrays.writeAll(stream, format.arrayLayout, write[M, S, A](_, _, format.nested, objectKeyOrder), elements)
      case Json.Object(elements, _) =>
        Objects.writeAll[M, S, ObjectEntry[A]](
          stream,
          format.objectLayout,
          { (stream, entry) => Strings.write(stream, entry.key)},
          { (stream, entry) => write(stream, entry.value, format.nested, objectKeyOrder)},
          objectKeyOrder(elements.values)
        )
    }



  /** Returns compact string representation of the given JSON. */
  def toCompactString(v: Json[?]): java.lang.String = {
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringWriter()
    Unnest.run(writeCompact(stream, v))
    stream.toString()
  }


  /** Returns pretty string representation of the given JSON. */
  def toPrettyString(
        v: Json[?],
        indent: Int = 2,
      ): java.lang.String = {
    import fun.instances.Unnest
    import fun.instances.Unnest.given

    val stream = new StringWriter()
    Unnest.run(writePretty(stream, v, indent))
    stream.toString()
  }
}
