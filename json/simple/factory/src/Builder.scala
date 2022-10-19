package io.github.maxkar
package json.simple

import json.parser.Values.SimpleBuilder

/**
 * Buidler of the simple JSON model.
 */
object Builder extends SimpleBuilder[Json]:
  override def fromBoolean(v: Boolean): Json =
    if v then Json.True else Json.False

  override def fromNull(): Json =
    Json.Null

  override def fromNumber(repr: String): Json =
    Json.Number(repr)

  override def fromString(v: String): Json =
    Json.String(v)

  override def fromArray(items: Seq[Json]): Json =
    Json.Array(items)

  override def fromObject(elems: Map[String, Json]): Json =
    Json.Object(elems)
end Builder
