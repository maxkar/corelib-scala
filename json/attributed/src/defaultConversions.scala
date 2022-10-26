package io.github.maxkar
package json.attr.defaultConversions

import json.attr.Json

/**
 * Conversion from JSON to boolean value.
 */
given jsonToBoolean[A]: Function[Json[A], Either[String, Boolean]] with
  override def apply(v: Json[A]): Either[String, Boolean] =
    v match
      case Json.True(_) => Right(true)
      case Json.False(_) => Right(false)
      case other => Left(s"Can't convert ${Json.typeName(other)} to boolean")
    end match
  end apply
end jsonToBoolean


/**
 * Conversion from JSON to string value.
 */
given jsonToString[A]: Function[Json[A], Either[String, String]] with
  override def apply(v: Json[A]): Either[String, String] =
    v match
      case Json.String(v, _) => Right(v)
      case other => Left(s"Can't convert ${Json.typeName(other)} to string")
    end match
  end apply
end jsonToString


/**
 * Conversion from JSON to int value.
 */
given jsonToInt[A]: Function[Json[A], Either[String, Int]] with
  override def apply(v: Json[A]): Either[String, Int] =
    v match
      case Json.Number(v, _) =>
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


/**
 * Conversion from JSON to long value.
 */
given jsonToLong[A]: Function[Json[A], Either[String, Long]] with
  override def apply(v: Json[A]): Either[String, Long] =
    v match
      case Json.Number(v, _) =>
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


/**
 * Conversion from JSON to float value.
 */
given jsonToFloat[A]: Function[Json[A], Either[String, Float]] with
  override def apply(v: Json[A]): Either[String, Float] =
    v match
      case Json.Number(v, _) =>
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


/**
 * Conversion from JSON to double value.
 */
given jsonToDouble[A]: Function[Json[A], Either[String, Double]] with
  override def apply(v: Json[A]): Either[String, Double] =
    v match
      case Json.Number(v, _) =>
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


/**
 * Conversion from JSON to BigDecimal.
 */
given jsonToBigInt[A]: Function[Json[A], Either[String, BigInt]] with
  override def apply(v: Json[A]): Either[String, BigInt] =
    v match
      case Json.Number(v, _) =>
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


/**
 * Conversion from JSON to BigDecimal.
 */
given jsonToBigDecimal[A]: Function[Json[A], Either[String, BigDecimal]] with
  override def apply(v: Json[A]): Either[String, BigDecimal] =
    v match
      case Json.Number(v, _) =>
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


/**
 * Noop conversion. Could be used to derive JSON value or optional JSON value
 * from a query.
 */
given noopConversion[A]: Function[Json[A], Either[String, Json[A]]] with
  override def apply(v: Json[A]): Either[String, Json[A]] = Right(v)
end noopConversion
