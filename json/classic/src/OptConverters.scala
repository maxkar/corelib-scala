package io.github.maxkar
package json.classic


/** Conversions for optional fields. */
trait OptConverters {

  /** Creates a conversion from json to an optional value based on non-optional conversion. */
  given jsonToOpt[T](using base: Conversion[Json, T]): Conversion[Json, Option[T]] =
    new Conversion[Json, Option[T]] {
      override def apply(v: Json): Option[T] =
        v match
          case Json.Null | Json.Undefined => None
          case other => Some(base(other))
    }


  /** Creates a conversion from an optional value to json based on non-optional conversion. */
  given optToJson[T](using base: Conversion[T, Json]): Conversion[Option[T], Json] =
    new Conversion[Option[T], Json] {
      override def apply(v: Option[T]): Json =
        v match
          case None => Json.Undefined
          case Some(x) => base(x)
    }
}
