package io.github.maxkar
package json.simple.query

import json.simple.Json
import json.query.Query

/**
 * Automatic derivation of the conversion from definitions for M (error encoding) and
 * Json => T conversion.
 */
given strictConversion[M[_], T](using pf: Function[Json, Either[String, T]], mcb: ConvertibleBy[M]): Conversion[Query[Json], M[T]] with
  override def apply(v: Query[Json]): M[T] =
    v match
      case Query.ValidQuery(path, vv) =>
        pf(vv) match
          case Right(v) => mcb.pure(v)
          case Left(err) => mcb.invalidDomainValue(path, vv, err)
        end match
      case Query.MissingElement(validPath, value, invalidPath) =>
        mcb.fieldMissing(validPath, value, invalidPath)
      case Query.InvalidSelector(validPath, value, invalidPath) =>
        mcb.accessError(validPath, value, invalidPath)
    end match
  end apply
end strictConversion

/**
 * Automatic derivation of the conversion from definitions for M (error encoding) and
 * Json => T conversion (for optional values).
 */
given optionalConversion[M[_], T](using pf: Function[Json, Either[String, T]], mcb: ConvertibleBy[M]): Conversion[Query[Json], M[Option[T]]] with
  override def apply(v: Query[Json]): M[Option[T]] =
    v match
      case Query.ValidQuery(path, vv) =>
        pf(vv) match
          case Right(v) => mcb.pure(Some(v))
          case Left(err) => mcb.invalidDomainValue(path, vv, err)
        end match
      case Query.MissingElement(validPath, value, invalidPath) =>
        mcb.pure(None)
      case Query.InvalidSelector(validPath, value, invalidPath) =>
        mcb.accessError(validPath, value, invalidPath)
    end match
  end apply
end optionalConversion
