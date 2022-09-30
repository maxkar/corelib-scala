package io.github.maxkar
package json.simple

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
end Json
