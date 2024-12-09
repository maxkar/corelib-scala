package io.github.maxkar
package json.classic

/** Base (primitive) JSON conversions. */
trait BaseConverters:

  /** Conversion from JSON to Boolean. */
  given jsonToBoolean: Conversion[Json, Boolean] =
    new Conversion[Json, Boolean] {
      override def apply(v: Json): Boolean =
        v match
          case Json.True => true
          case Json.False => false
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to boolean")
    }

  /** Conversion from Boolean to JSON. */
  given booleanToJson: Conversion[Boolean, Json] =
    new Conversion[Boolean, Json] {
      override def apply(v: Boolean): Json =
        if v then Json.True else Json.False
    }



  /** Conversion from JSON to String. */
  given jsonToString: Conversion[Json, String] =
    new Conversion[Json, String] {
      override def apply(v: Json): String =
        v match
          case Json.String(x) => x
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to string")
    }

  /** Conversion from String to JSON. */
  given stringToJson: Conversion[String, Json] =
    new Conversion[String, Json] {
      override def apply(v: String): Json =
        if v == null then Json.Null else Json.String(v)
    }



  /** Conversion from JSON to Int. */
  given jsonToInt: Conversion[Json, Int] =
    new Conversion[Json, Int] {
      override def apply(v: Json): Int =
        v match
          case Json.Number(n) =>
            try
              Integer.parseInt(n)
            catch
              case e: NumberFormatException =>
                throw Json.Exception(s"${n} is not valid integer")
            end try
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to int")
    }

  /** Conversion from Int to JSON. */
  given intToJson: Conversion[Int, Json] =
    new Conversion[Int, Json] {
      override def apply(v: Int): Json = Json.Number(v.toString())
    }



  /** Conversion from JSON to Long. */
  given jsonToLong: Conversion[Json, Long] =
    new Conversion[Json, Long] {
      override def apply(v: Json): Long =
        v match
          case Json.Number(n) =>
            try
              java.lang.Long.parseLong(n)
            catch
              case e: NumberFormatException =>
                throw Json.Exception(s"${n} is not valid long integer")
            end try
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to long int")
    }

  /** Conversion from Long to JSON. */
  given longToJson: Conversion[Long, Json] =
    new Conversion[Long, Json] {
      override def apply(v: Long): Json = Json.Number(v.toString())
    }



  /** Conversion from JSON to Double. */
  given jsonToDouble: Conversion[Json, Double] =
    new Conversion[Json, Double] {
      override def apply(v: Json): Double =
        v match
          case Json.Number(n) =>
            try
              java.lang.Double.parseDouble(n)
            catch
              case e: NumberFormatException =>
                throw Json.Exception(s"${n} is not valid doulbe")
            end try
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to doulbe")
    }

  /** Conversion from Double to JSON. */
  given doubleToJson: Conversion[Double, Json] =
    new Conversion[Double, Json] {
      override def apply(v: Double): Json = Json.Number(v.toString())
    }



  /** Conversion from JSON to BigInt */
  given jsonToBigint: Conversion[Json, BigInt] =
    new Conversion[Json, BigInt] {
      override def apply(v: Json): BigInt =
        v match
          case Json.Number(n) =>
            try
              BigInt(n)
            catch
              case e: NumberFormatException =>
                throw Json.Exception(s"${n} is not valid bigint")
            end try
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to bigint")
    }

  /** Conversion from BigInt to JSON. */
  given bigintToJson: Conversion[BigInt, Json] =
    new Conversion[BigInt, Json] {
      override def apply(v: BigInt): Json =
        if v == null then Json.Null else Json.Number(v.toString())
    }



  /** Conversion from JSON to BigDecimal */
  given jsonToBigdecimal: Conversion[Json, BigDecimal] =
    new Conversion[Json, BigDecimal] {
      override def apply(v: Json): BigDecimal =
        v match
          case Json.Number(n) =>
            try
              BigDecimal(n)
            catch
              case e: NumberFormatException =>
                throw Json.Exception(s"${n} is not valid bigdecimal")
            end try
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to bigdecimal")
    }

  /** Conversion from BigDecimal to JSON. */
  given bigdecimalToJson: Conversion[BigDecimal, Json] =
    new Conversion[BigDecimal, Json] {
      override def apply(v: BigDecimal): Json =
        if v == null then Json.Null else Json.Number(v.toString())
    }



  /** Converts JSON into an optional value. Decodes Undefined/Null to None and keeps the rest as Same(x). */
  given jsonToOptJson: Conversion[Json, Option[Json]] =
    new Conversion[Json, Option[Json]] {
      override def apply(v: Json): Option[Json] =
        v match
          case Json.Undefined | Json.Null => None
          case other => Some(other)
    }

  /** Converts an optional value into JSON. The None is represented as Undefined. */
  given optJsonToJson: Conversion[Option[Json], Json] =
    new Conversion[Option[Json], Json] {
      override def apply(v: Option[Json]): Json =
        v match
          case None => Json.Undefined
          case Some(x) => x
    }



  /** Conversion from JSON to Json Array. */
  given jsonToJsonArray: Conversion[Json, Json.Array] =
    new Conversion[Json, Json.Array] {
      override def apply(v: Json): Json.Array =
        v match
          case a@Json.Array(_) => a
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to array")
    }

  /** Conversion from JSON to a Seqence. */
  given jsonToSeq: Conversion[Json, Seq[Json]] =
    new Conversion[Json, Seq[Json]] {
      override def apply(v: Json): Seq[Json] =
        v match
          case Json.Array(a) => a
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to array")
    }

  /** Conversion from JSON sequence to JSON. */
  given seqToJson: Conversion[Seq[Json], Json] =
    new Conversion[Seq[Json], Json] {
      override def apply(v: Seq[Json]): Json = Json.array(v*)
    }



  /** Conversion from JSON to a Json Object. */
  given jsonToJsonObject: Conversion[Json, Json.Object] =
    new Conversion[Json, Json.Object] {
      override def apply(v: Json): Json.Object =
        v match
          case a@Json.Object(_) => a
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to object")
    }

  /** Conversion from JSON to a simple map. */
  given jsonToMap: Conversion[Json, Map[String, Json]] =
    new Conversion[Json, Map[String, Json]] {
      override def apply(v: Json): Map[String, Json] =
        v match
          case Json.Object(a) => a
          case other => throw Json.Exception(s"Can't convert ${Json.typeName(other)} to object")
    }

  /** Conversion from a (simple) map to json. */
  given mapToJson: Conversion[Map[String, Json], Json] =
    new Conversion[Map[String, Json], Json] {
      override def apply(v: Map[String, Json]): Json =
        Json.make(v.toSeq*)
    }
end BaseConverters
