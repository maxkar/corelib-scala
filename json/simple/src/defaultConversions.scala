package io.github.maxkar
package json.simple.defaultConversions

import json.simple.Json

/**
 * Conversion from JSON to boolean value.
 */
given jsonToBoolean: Function[Json, Either[String, Boolean]] with
  override def apply(v: Json): Either[String, Boolean] =
    v match
      case Json.True => Right(true)
      case Json.False => Right(false)
      case other => Left(s"Can't convert ${Json.typeName(other)} to boolean")
    end match
  end apply
end jsonToBoolean


/** Conversion from boolean to json. */
given booleanToJson: Conversion[Boolean, Json] with
  override def apply(x: Boolean): Json =
    if x then Json.True else Json.False
end booleanToJson


/**
 * Conversion from JSON to string value.
 */
given jsonToString: Function[Json, Either[String, String]] with
  override def apply(v: Json): Either[String, String] =
    v match
      case Json.String(v) => Right(v)
      case other => Left(s"Can't convert ${Json.typeName(other)} to string")
    end match
  end apply
end jsonToString


/** Conversion from string to json. */
given stringToJson: Conversion[String, Json] with
  override def apply(x: String): Json = Json.String(x)
end stringToJson


/**
 * Conversion from JSON to int value.
 */
given jsonToInt: Function[Json, Either[String, Int]] with
  override def apply(v: Json): Either[String, Int] =
    v match
      case Json.Number(v) =>
        try
          Right(Integer.parseInt(v))
        catch
          case e: NumberFormatException =>
            Left(s"Can't convert ${v} to int")
        end try
      case other => Left(s"Can't convert ${Json.typeName(other)} to int")
    end match
  end apply
end jsonToInt

/** Converts int to json. */
given intToJson: Conversion[Int, Json] with
  override def apply(x: Int): Json = Json.Number(x.toString())
end intToJson


/**
 * Conversion from JSON to long value.
 */
given jsonToLong: Function[Json, Either[String, Long]] with
  override def apply(v: Json): Either[String, Long] =
    v match
      case Json.Number(v) =>
        try
          Right(java.lang.Long.parseLong(v))
        catch
          case e: NumberFormatException =>
            Left(s"Can't convert ${v} to long")
        end try
      case other => Left(s"Can't convert ${Json.typeName(other)} to long")
    end match
  end apply
end jsonToLong

/** Converts long to json. */
given longToJson: Conversion[Long, Json] with
  override def apply(x: Long): Json = Json.Number(x.toString())
end longToJson


/**
 * Conversion from JSON to float value.
 */
given jsonToFloat: Function[Json, Either[String, Float]] with
  override def apply(v: Json): Either[String, Float] =
    v match
      case Json.Number(v) =>
        try
          Right(java.lang.Float.parseFloat(v))
        catch
          case e: NumberFormatException =>
            Left(s"Can't convert ${v} to float")
        end try
      case other => Left(s"Can't convert ${Json.typeName(other)} to float")
    end match
  end apply
end jsonToFloat

/** Converts float to json. */
given floatToJson: Conversion[Float, Json] with
  override def apply(x: Float): Json = Json.Number(x.toString())
end floatToJson

/**
 * Conversion from JSON to double value.
 */
given jsonToDouble: Function[Json, Either[String, Double]] with
  override def apply(v: Json): Either[String, Double] =
    v match
      case Json.Number(v) =>
        try
          Right(java.lang.Double.parseDouble(v))
        catch
          case e: NumberFormatException =>
            Left(s"Can't convert ${v} to double")
        end try
      case other => Left(s"Can't convert ${Json.typeName(other)} to double")
    end match
  end apply
end jsonToDouble

/** Converts double to json. */
given doubleToJson: Conversion[Double, Json] with
  override def apply(x: Double): Json = Json.Number(x.toString())
end doubleToJson


/**
 * Conversion from JSON to BigInt.
 */
given jsonToBigInt: Function[Json, Either[String, BigInt]] with
  override def apply(v: Json): Either[String, BigInt] =
    v match
      case Json.Number(v) =>
        try
          Right(BigInt(v))
        catch
          case e: NumberFormatException =>
            Left(s"Can't convert ${v} to (big) integer")
        end try
      case other => Left(s"Can't convert ${Json.typeName(other)} to (big) integer")
    end match
  end apply
end jsonToBigInt

/** Converts BigInt to json. */
given bigIntToJson: Conversion[BigInt, Json] with
  override def apply(x: BigInt): Json = Json.Number(x.toString())
end bigIntToJson

/**
 * Conversion from JSON to BigDecimal.
 */
given jsonToBigDecimal: Function[Json, Either[String, BigDecimal]] with
  override def apply(v: Json): Either[String, BigDecimal] =
    v match
      case Json.Number(v) =>
        try
          Right(BigDecimal(v))
        catch
          case e: NumberFormatException =>
            Left(s"Can't convert ${v} to (big) decimal")
        end try
      case other => Left(s"Can't convert ${Json.typeName(other)} to (big) decimal")
    end match
  end apply
end jsonToBigDecimal

/** Converts BigDecimal to json. */
given bigDecimalToJson: Conversion[BigDecimal, Json] with
  override def apply(x: BigDecimal): Json = Json.Number(x.toString())
end bigDecimalToJson

/**
 * Noop conversion. Could be used to derive JSON value or optional JSON value
 * from a query.
 */
given noopConversion: Function[Json, Either[String, Json]] with
  override def apply(v: Json): Either[String, Json] = Right(v)
end noopConversion
