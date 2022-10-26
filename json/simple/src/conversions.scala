package io.github.maxkar
package json.simple

import json.simple.Json
import json.query.Query
import json.query.Path

import fun.typeclass.Collect
import fun.typeclass.Monad

import scala.language.implicitConversions


/**
 * Automatic derivation of the conversion from definitions for M (error encoding) and
 * Json => T conversion.
 */
given strictConversion[M[_], T](
      using pf: Function[Json, Either[String, T]],
      mcb: ConvertibleBy[M])
    : Conversion[Query[Json], M[T]] with
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

inline given strictIdConversion[T](
      using pf: Function[Json, Either[String, T]],
      mcb: ConvertibleBy[({type Id[T] = T})#Id])
    : Conversion[Query[Json], T]
  = strictConversion


/**
 * Automatic derivation of the conversion from definitions for M (error encoding) and
 * Json => T conversion (for optional values).
 */
given optionalConversion[M[_], T](
      using pf: Function[Json, Either[String, T]],
      mcb: ConvertibleBy[M])
    : Conversion[Query[Json], M[Option[T]]] with
  override def apply(v: Query[Json]): M[Option[T]] =
    v match
      case Query.ValidQuery(_, Json.Null) => mcb.pure(None)
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

inline given optionalIdConversion[T](
      using pf: Function[Json, Either[String, T]],
      mcb: ConvertibleBy[({type Id[T] = T})#Id])
    : Conversion[Query[Json], Option[T]]
  = optionalConversion


/**
 * Automatic conversion from optional value to MaybeValue that could
 * be used in update/construct operations for JSON objects and arrays.
 */
given optionalConstruction[T, S <: MaybeJson](using c: Conversion[T, S])
    : Conversion[Option[T], MaybeJson] with
  override def apply(x: Option[T]): MaybeJson =
    x match
      case None => Json.Empty
      case Some(x) => c(x)
    end match
  end apply
end optionalConstruction


/**
 * Automatic conversion Query -> Seq[T] based on conversions
 * Query -> T and Query -> Seq[Query].
 */
given sequenceConversion[M[_]: Collect: Monad, T](
      using eltConv: Conversion[Query[Json], M[T]],
      seqConv: Conversion[Query[Json], M[Seq[Query[Json]]]])
    : Conversion[Query[Json], M[Seq[T]]] with
  override def apply(v: Query[Json]): M[Seq[T]] =
    for {
      items <- seqConv(v)
      res <- Collect.seq(items.map(eltConv))
    } yield res

end sequenceConversion


inline given sequenceIdConversion[T](
      using eltConv: Conversion[Query[Json], T],
      seqConv: Conversion[Query[Json], Seq[Query[Json]]])
    : Conversion[Query[Json], Seq[T]] with
  override def apply(v: Query[Json]): Seq[T] =
    seqConv(v).map(eltConv)
end sequenceIdConversion


/** Automatic construction of JSON arrays (values) from simple values. */
given sequenceConstruction[T <: MaybeJson]: Conversion[Seq[T], Json.Array] with
  override def apply(x: Seq[T]): Json.Array =
    Json.array(x: _*)
end sequenceConstruction


/**
 * Automatic conversion Query -> Map[String, T] based on conversions
 * Query -> T and Query -> Seq[Map[String, Query]].
 */
given mapConversion[M[_]: Collect: Monad, T](
      using eltConv: Conversion[Query[Json], M[T]],
      mapConv: Conversion[Query[Json], M[Map[String, Query[Json]]]])
    : Conversion[Query[Json], M[Map[String, T]]] with
  override def apply(v: Query[Json]): M[Map[String, T]] =
    for {
      items <- mapConv(v)
      res <- Collect.seq {
        for {
          (k, v) <- items.toSeq
        } yield
          for {
            converted <- eltConv(v)
          } yield
            (k, converted)
      }
    } yield res.toMap

end mapConversion


given mapIdConversion[T](
      using eltConv: Conversion[Query[Json], T],
      mapConv: Conversion[Query[Json], Map[String, Query[Json]]])
    : Conversion[Query[Json], Map[String, T]] with
  override def apply(v: Query[Json]): Map[String, T] =
    mapConv(v).view.mapValues(eltConv).toMap
end mapIdConversion


/** Automatic construction of JSON objects from the appropriate scala maps. */
given mapConstruction[T <: MaybeJson]: Conversion[Map[String, T], Json.Object] with
  override def apply(x: Map[String, T]): Json.Object =
    Json.makeFrom(x.iterator)
end mapConstruction



/** Conversion from Query to optional query. */
given queryToOptionalQuery: Conversion[Query[Json], Option[Query[Json]]] with
  override def apply(v: Query[Json]): Option[Query[Json]] =
    v match
      case Query.MissingElement(validPath, value, invalidPath) => None
      case other => Some(other)
    end match
  end apply
end queryToOptionalQuery


/**
 * Conversion from Query to sequence of queries.
 */
given queryToSequenceConversion[M[_]](using mcb: ConvertibleBy[M])
    : Conversion[Query[Json], M[Seq[Query[Json]]]] with
  override def apply(v: Query[Json]): M[Seq[Query[Json]]] =
    v match
      case Query.ValidQuery(path, Json.Array(elements)) =>
        val content =
          for
            (elem, idx) <- elements.zipWithIndex
          yield
            Query.ValidQuery(path / idx, elem)
        mcb.pure(content.toSeq)
      case Query.ValidQuery(path, nonArray) =>
        mcb.invalidDomainValue(path, nonArray, "Array expected")
      case Query.MissingElement(validPath, value, invalidPath) =>
        mcb.fieldMissing(validPath, value, invalidPath)
      case Query.InvalidSelector(validPath, value, invalidPath) =>
        mcb.accessError(validPath, value, invalidPath)
    end match
  end apply
end queryToSequenceConversion

inline given queryToSequenceIdConversion(
      using mcb: ConvertibleBy[({type Id[T] = T})#Id])
    : Conversion[Query[Json], Seq[Query[Json]]] =
  queryToSequenceConversion

/** Automatic construction of JSON arrays (values) from convertible values. */
given deepSequenceConstruction[T, S <: MaybeJson](
      using c: Conversion[T, S])
    : Conversion[Seq[T], Json.Array] with
  override def apply(x: Seq[T]): Json.Array =
    x.map(c)
end deepSequenceConstruction


/**
 * Conversion from Query to map with queries as values.
 */
given queryToMapConversion[M[_]](using mcb: ConvertibleBy[M])
    : Conversion[Query[Json], M[Map[String, Query[Json]]]] with
  override def apply(v: Query[Json]): M[Map[String, Query[Json]]] =
    v match
      case Query.ValidQuery(path, Json.Object(elements)) =>
        val content =
          for
            (key, value) <- elements
          yield
            (key -> Query.ValidQuery(path / key, value))
        mcb.pure(content.toMap)
      case Query.ValidQuery(path, nonArray) =>
        mcb.invalidDomainValue(path, nonArray, "Object expected")
      case Query.MissingElement(validPath, value, invalidPath) =>
        mcb.fieldMissing(validPath, value, invalidPath)
      case Query.InvalidSelector(validPath, value, invalidPath) =>
        mcb.accessError(validPath, value, invalidPath)
    end match
  end apply
end queryToMapConversion


inline given queryToMapIdConversion(
      using mcb: ConvertibleBy[({type Id[T] = T})#Id])
    : Conversion[Query[Json], Map[String, Query[Json]]] =
  queryToMapConversion

/** Automatic construction of JSON objects (values) from convertible maps. */
given deepObjectConstruction[T, S <: MaybeJson](
      using c: Conversion[T, S])
    : Conversion[Map[String, T], Json.Object] with
  override def apply(x: Map[String, T]): Json.Object =
    Json.makeFrom(x.view.mapValues(c).iterator)
end deepObjectConstruction
