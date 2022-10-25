package io.github.maxkar
package json.simple.writer

import json.simple.Json
import json.writer.Values

/**
 * Implementation of the write protocol for the simple JSON.
 */
given WriteableFactory:  Values.ValueClassifier[Json] with

  override def classifyValue[R](jsonValue: Json, visitor: Values.ValueCallback[Json, R]): R =
    jsonValue match
      case Json.Null => visitor.nullValue()
      case Json.True => visitor.boolean(true)
      case Json.False => visitor.boolean(false)
      case Json.String(value) => visitor.string(value)
      case Json.Number(representation) => visitor.number(representation)
      case Json.Array(elts) => visitor.array(elts.iterator)
      case Json.Object(elts) => visitor.unorderedObject(elts.iterator)
      end match
  end classifyValue
end WriteableFactory


