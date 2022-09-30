package io.github.maxkar
package json.attr

/**
 * Single node in the JSON tree model.
 * @tparam A type of the attributes that are applied to json.
 */
abstract sealed class Json[+A]:
  /** Attributes of this JSON node. */
  val attrs: A

  /**
   * Builds a new json by applying function to attributes of this json
   * element and attributes of nested elemnts (if any). The calculation
   * is eager.
   * @param fn function to apply to json attributes.
   */
  def mapAttributes[R](fn: A => R): Json[R]
end Json


object Json:
  /**
   * Information about one entry in the json object. It captures information
   * related to the object's key along with the value associated with the key.
   *
   * @param key key of the entry.
   * @param keyAttrs attributes applicable to the entry's key.
   * @param value value of the given entry.
   */
  case class ObjectEntry[+A](key: java.lang.String, keyAttrs: A, value: Json[A]):
    /**
     * Builds a new json entry by applying function to both key's and value's attributes.
     * @param fn function to apply to json attributes.
     */
    def mapAttributes[R](fn: A => R): ObjectEntry[R] =
      ObjectEntry(key, fn(keyAttrs), value.mapAttributes(fn))

  end ObjectEntry


  /** Representation of the json "null" literal. */
  case class Null[+A](attrs: A) extends Json[A]:
    override def mapAttributes[R](fn: A => R): Json[R] =
      Null(fn(attrs))
  end Null

  /** Representation of the json "true" literal. */
  case class True[+A](attrs: A) extends Json[A]:
    override def mapAttributes[R](fn: A => R): Json[R] =
      True(fn(attrs))
  end True

  /** Representation of the json "false" literal. */
  case class False[+A](attrs: A) extends Json[A]:
    override def mapAttributes[R](fn: A => R): Json[R] =
      True(fn(attrs))
  end False

  /**
   * JSON string literal.
   * @param value value of the literal.
   */
  case class String[+A](value: java.lang.String, attrs: A) extends Json[A]:
    override def mapAttributes[R](fn: A => R): Json[R] =
      String(value, fn(attrs))
  end String

  /**
   * String representation of the JSON numeric literal. The node contains
   * "raw" (unparesd) value but ensures it is well-formed number according
   * to the JSON.
   * @param value raw (unparsed) value.
   */
  case class Number[+A](value: java.lang.String, attrs: A) extends Json[A]:
    override def mapAttributes[R](fn: A => R): Json[R] =
      String(value, fn(attrs))
  end Number


  /**
   * Json array.
   * @param elements elemenst of the JSON array.
   * @param attrs attributes applicable to this array.
   */
  case class Array[+A](elements: Seq[Json[A]], attrs: A) extends Json[A]:
    /** Retrieves nth element of the array. */
    def apply(index: Int): Json[A] = elements(index)

    override def mapAttributes[R](fn: A => R): Json[R] =
      Array(elements.map(_.mapAttributes(fn)), fn(attrs))
  end Array


  /**
   * Json object.
   * @param elements entries of the object (object keys must match map entry keys).
   * @param attrs attributes of this json value (complete object).
   */
  case class Object[+A](elements: Map[java.lang.String, ObjectEntry[A]], attrs: A) extends Json[A]:
    /** Retrieves value associated with the given key. */
    def apply(key: java.lang.String): Json[A] = elements(key).value

    /** Retrieves optional value associated with the key. */
    def get(key: java.lang.String): Option[Json[A]] = elements.get(key).map(_.value)

    override def mapAttributes[R](fn: A => R): Json[R] =
      Object(elements.view.mapValues(_.mapAttributes(fn)).toMap, fn(attrs))
  end Object


  /**
   * Returns simple name of the JSON type.
   */
  def typeName(v: Json[_]): java.lang.String =
    v match
      case Json.Null(_) => "null"
      case Json.True(_) | Json.False(_) => "boolean"
      case Json.String(_, _) => "string"
      case Json.Number(_, _) => "number"
      case Json.Array(_, _) => "array"
      case Json.Object(_, _) => "object"
    end match
  end typeName
end Json
