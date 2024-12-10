package io.github.maxkar
package json.classic

/** Converters for maps. */
trait MapConverters {
  /** Converts JSON into a simple map if there is an element conversion. */
  given jsonToMap[T](
        using base: Conversion[Json, T],
        m: Conversion[Json, Map[String, Json]]
      ): Conversion[Json, Map[String, T]] =
    new Conversion[Json, Map[String, T]] {
      override def apply(v: Json): Map[String, T] =
        m(v).view.mapValues(base).toMap
    }


  /** Converts a simple map into a json collection if there is an element conversion. */
  given mapToJson[T](
        using base: Conversion[T, Json],
        m: Conversion[Map[String, Json], Json]
      ): Conversion[Map[String, T], Json] =
    new Conversion[Map[String, T], Json] {
      override def apply(v: Map[String, T]): Json =
        m(v.view.mapValues(base).toMap)
    }
}
