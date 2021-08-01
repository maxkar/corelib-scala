package io.github.maxkar
package json.classic

import java.io.IOException

import scala.language.dynamics


/** A real JSON value or a placeholder for absent (non-existing) value.  */
abstract sealed class Json extends Dynamic:
  /**
   * Retrieves a child of this value considering it is a json object. Returns
   * {{{Json.Undefined}}} if this value is not an object or the name is not defined
   * on the current value.
   * @param name field name.
   * @return field of the object or {{{Json.Undefined}}} when key not found or this 
   *   value is not json object.
   */
  def selectDynamic(name: java.lang.String): Json = Json.Undefined


  /**
   * Synonym for the {{{selectDynamic}}} method. Useful when key is not
   * valid scala identifier or when index access is used after field access.
   */
  inline def apply(name: java.lang.String): Json =
    selectDynamic(name)


  /**
   * Selects a value by index. Returns {{{Json.Undefined}} if this value is not a
   * {{{JsonValue.Array}}} or if the index is out of bounds. 
   * @param idx index of the value to select.
   * @return value at the given index or {{{Json.Undefined}}} when this value is not
   *   an array or index is out of bounds.
   */
  def apply(idx: Int): Json = Json.Undefined


  /**
   * Converts this value into the target type (if possible).
   * @throws Json.Exception if value could not be parsed as the target type.
   */
  inline def as[T](using conv: Conversion[Json, T]): T =
    conv(this)


  /** Convenience conversion to a string representation. */
  override final def toString(): java.lang.String =
    Json.toString(this)

end Json



/**
 * The main entry point to the json-related functionality. 
 * Contains specific json types and provides access to parsing and serialization functionality.
 */
object Json:
  /**
   * Exception in JSON handling or processing. This exception is only raised
   * by parsing and conversion functions (and never by JSON generation functions).
   * Json manipulations usually occur as part of input and output hence the 
   * inheritance from IOException.
   */
  class Exception(msg: java.lang.String) extends IOException(msg) 


  /**
   * Undefined (non-existing) value. It is returned from member selectors when
   * a field does not exist on an object or index is out of array bounds. Object and
   * Array constructors ignore undefined elements. A json {{{null}}} literal is 
   * considered to be some defined value and is represented by the {{{Null}}} constant.
   */
  case object Undefined extends Json


  /**
   * JSON null literal. Unlike {{{Undefined}}} this is a real value which is
   * could be read or written.
   */
  case object Null extends Json


  /** JSON `true` literal. */
  case object True extends Json


  /** JSON `false` literal. */
  case object False extends Json


  /**
   * JSON string literal. The value is an unquoted (effective) string.
   * @param value value of the literal.
   */
  case class String(v: java.lang.String) extends Json


  /**
   * JSON number literal. It contains unparsed (raw) string representation of
   * a number (the representation has to be well-formed). Client code could
   * decide how numbers should be converted (with or without precision loss)
   * depending on the context.
   */
  case class Number(value: java.lang.String) extends Json:
    Number.validate(value)


  /** 
   * JSON array (a sequence of JSON values). The instance constructor does not
   * accept {{{Undefined}}} values, use {{{Json.array}}} if this may be the case.
   * @param elements elements of the array.
   */
  case class Array(elements: Seq[Json]) extends Json:
    if elements.contains(Undefined) then
      throw IllegalArgumentException("Json array could not contain Undefined elements")

    override def apply(idx: Int): Json =
      if idx < 0 || idx >= elements.length then
        Undefined
      else
        elements(idx)


    /** Concatenates this array with elements from another array. */
    def ++(other: Array): Array = Array(elements ++ other.elements)

  end Array


  /**
   * JSON object (an unordered collection of key-value pairs). The instance constructor
   * does not accept {{{Undefined}}} values, use {{{Json.make}}} if this may be the case.
   * @param values map from keys to corresponding values.
   */
  final case class Object(values: Map[java.lang.String, Json]) extends Json:
    if values.values.iterator.contains(Undefined) then
      throw IllegalArgumentException("Undefined values are not allowed in Json object")

    override def selectDynamic(name: java.lang.String): Json =
      values.getOrElse(name, Undefined)

    /** Combines two objects into one. */
    def ++(other: Object): Object = Object(values ++ other.values)

  end Object

  
  /** Returns a human-readable name of the type of the json value. */
  def typeName(v: Json): java.lang.String =
    v match {
      case Undefined => "undefined"
      case Null => "null"
      case True | False => "boolean"
      case String(_) => "string"
      case Number(_) => "number"
      case Array(_) => "array"
      case Object(_) => "object"
    }


  /**
   * Creates a JSON array from a list (of possibly optional) values. 
   * {{{Undefined}}} values are removed from the input. {{{Null}}} values
   * remains as null liteterals.
   */
  def array(items: Json*): Array =
    Array(items.filter(_ != Undefined))


  /**
   * Creates a new JSON object from a list of key-value pairs. Pairs with the
   * {{{Undefined}}} values are ignored (not present in the output). The {{{Null}}}
   * values are serialized as Json literals.
   */
  def make(pairs: (java.lang.String, Json)*): Object =
    Object(pairs.filter(x => x._2 != Undefined).toMap)


  /**
   * Writes the JSON value into the target stream.
   * @param value value to write. Could not be {{{Undefined}}}.
   * @param stream output writer to put the value to.
   * @throws IOException if an error happens.
   * @throws JsonException if value is undefined.
   */
  def write(value: Json, stream: java.io.Writer): Unit = 
    Serializer.write(value, stream)


  /** Converts a JSON value into a string. */
  def toString(value: Json): java.lang.String =
    val sw = java.io.StringWriter()
    write(value, sw)
    sw.close()
    sw.toString()


  /** Parses JSON from a reader. */
  def parse(r: java.io.Reader): Json = 
    Deserializer.parse(r)


  /** Parses JSON from a string. */
  def parse(str: java.lang.String): Json =
    Deserializer.parse(new java.io.StringReader(str))


  /** Parses JSON from an array of bytes (assuming UTF-8 encodig per RFC 8259) */
  def parse(data: scala.Array[Byte]): Json =
    Deserializer.parse(data)


  /** 
   * Parses a byte array automatically detecting JSON encoding.
   * The RFC 8259 makes UTF-8 the only supported format thus the default read method change.
   */
  def parseRFC7158(data: scala.Array[Byte]): Json =
    Deserializer.parseRFC7158(data)


  /** Utilities for json number. */
  object Number:
    /** Optional minus sign. */
    private val patMaybeMinus = "-?"
    /** Pattern for the JSON integer value part. */
    private val patInt = "0|([1-9]\\d*)"
    /** Patters for the fractional part. */
    private val patFrac = "\\.\\d+"
    /** Pattern for the exponent part. */
    private val patExp = "[eE][-+]?\\d+"

    /** Pattern for the json number. */
    private val pat = 
      java.util.regex.Pattern.compile(
        s"${patMaybeMinus}(${patInt})(${patFrac})?(${patExp})?"
      )


    /** 
     * Validates that the number is a valid JSON number representation.
     * @throws IllegalArgumentException if value is not valid.
     */
    private def validate(num: java.lang.String): Unit =
      if !pat.matcher(num).matches then
        /* Json parser explicitly raises JsonException, this one is always developer error so IAE. */
        throw IllegalArgumentException("Invalid JSON number value " + num)

  end Number

end Json
