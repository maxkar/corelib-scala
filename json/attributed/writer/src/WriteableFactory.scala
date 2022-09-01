package io.github.maxkar
package json.attr.writer

import json.attr.Json
import json.writer.Writeable
import json.writer.ValueVisitor

/**
 * Implementation of the write protocol for the attributed JSON.
 */
given WriteableFactory:  Writeable[Json[Any]] with

  override def decodeElement[R](jsonValue: Json[Any], visitor: ValueVisitor[Json[Any], R]): R =
    jsonValue match
      case Json.Null(_) => visitor.nullValue()
      case Json.True(_) => visitor.boolean(true)
      case Json.False(_) => visitor.boolean(false)
      case Json.String(value, _) => visitor.string(value)
      case Json.Number(representation, _) => visitor.number(representation)
      case Json.Array(elts, _) => visitor.array(elts.iterator)
      case Json.Object(elts, _) =>
        visitor.unorderedObject(
          elts.view.mapValues(_.value).iterator
        )
      end match
  end decodeElement
end WriteableFactory
