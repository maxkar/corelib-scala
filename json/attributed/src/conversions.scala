package io.github.maxkar
package json.attr

import json.query.Query
import json.query.Path

import fun.typeclass.Collect
import fun.typeclass.Monad

import scala.language.implicitConversions


/**
 * Automatic derivation of the conversion from definitions for M (error encoding) and
 * Json => T conversion.
 */
given strictConversion[M[_], A, T](
      using pf: Function[Json[A], Either[String, T]],
      mcb: ConvertibleBy[M, A])
    : Conversion[Query[Json[A]], M[T]] with {
  override def apply(v: Query[Json[A]]): M[T] =
    v match {
      case Query.ValidQuery(path, vv) =>
        pf(vv) match
          case Right(v) => mcb.pure(v)
          case Left(err) => mcb.invalidDomainValue(path, vv, err)
        end match
      case Query.MissingElement(validPath, value, invalidPath) =>
        mcb.fieldMissing(validPath, value, invalidPath)
      case Query.InvalidSelector(validPath, value, invalidPath) =>
        mcb.accessError(validPath, value, invalidPath)
    }
}

inline given strictIdConversion[A, T](
      using pf: Function[Json[A], Either[String, T]],
      mcb: ConvertibleBy[({type Id[T] = T})#Id, A])
    : Conversion[Query[Json[A]], T]
  = strictConversion


/**
 * Automatic derivation of the conversion from definitions for M (error encoding) and
 * Json => T conversion (for optional values).
 */
given optionalConversion[M[_], A, T](
      using pf: Function[Json[A], Either[String, T]],
      mcb: ConvertibleBy[M, A])
    : Conversion[Query[Json[A]], M[Option[T]]] with {
  override def apply(v: Query[Json[A]]): M[Option[T]] =
    v match {
      case Query.ValidQuery(_, Json.Null(_)) => mcb.pure(None)
      case Query.ValidQuery(path, vv) =>
        pf(vv) match
          case Right(v) => mcb.pure(Some(v))
          case Left(err) => mcb.invalidDomainValue(path, vv, err)
        end match
      case Query.MissingElement(validPath, value, invalidPath) =>
        mcb.pure(None)
      case Query.InvalidSelector(validPath, value, invalidPath) =>
        mcb.accessError(validPath, value, invalidPath)
    }
}

inline given optionalIdConversion[A, T](
      using pf: Function[Json[A], Either[String, T]],
      mcb: ConvertibleBy[({type Id[T] = T})#Id, A])
    : Conversion[Query[Json[A]], Option[T]]
  = optionalConversion


/**
 * Automatic conversion Query -> Seq[T] based on conversions
 * Query -> T and Query -> Seq[Query].
 */
given sequenceConversion[M[_]: Collect: Monad, A, T](
      using eltConv: Conversion[Query[Json[A]], M[T]],
      seqConv: Conversion[Query[Json[A]], M[Seq[Query[Json[A]]]]])
    : Conversion[Query[Json[A]], M[Seq[T]]] with {
  override def apply(v: Query[Json[A]]): M[Seq[T]] =
    for {
      items <- seqConv(v)
      res <- Collect.seq(items.map(eltConv))
    } yield res
}


inline given sequenceIdConversion[A, T](
      using eltConv: Conversion[Query[Json[A]], T],
      seqConv: Conversion[Query[Json[A]], Seq[Query[Json[A]]]])
    : Conversion[Query[Json[A]], Seq[T]] with {
  override def apply(v: Query[Json[A]]): Seq[T] =
    seqConv(v).map(eltConv)
}

/**
 * Automatic conversion Query -> Map[String, T] based on conversions
 * Query -> T and Query -> Seq[Map[String, Query]].
 */
given mapConversion[M[_]: Collect: Monad, A, T](
      using eltConv: Conversion[Query[Json[A]], M[T]],
      mapConv: Conversion[Query[Json[A]], M[Map[String, Query[Json[A]]]]])
    : Conversion[Query[Json[A]], M[Map[String, T]]] with {
  override def apply(v: Query[Json[A]]): M[Map[String, T]] =
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
}


given mapIdConversion[A, T](
      using eltConv: Conversion[Query[Json[A]], T],
      mapConv: Conversion[Query[Json[A]], Map[String, Query[Json[A]]]])
    : Conversion[Query[Json[A]], Map[String, T]] with {
  override def apply(v: Query[Json[A]]): Map[String, T] =
    mapConv(v).view.mapValues(eltConv).toMap
}

/**
 * Conversion from Query to optional query.
 */
given queryToOptionalQuery[A]: Conversion[Query[Json[A]], Option[Query[Json[A]]]] with {
  override def apply(v: Query[Json[A]]): Option[Query[Json[A]]] =
    v match {
      case Query.MissingElement(validPath, value, invalidPath) => None
      case other => Some(other)
    }
}


/**
 * Conversion from Query to sequence of queries.
 */
given queryToSequenceConversion[M[_], A](using mcb: ConvertibleBy[M, A])
    : Conversion[Query[Json[A]], M[Seq[Query[Json[A]]]]] with {
  override def apply(v: Query[Json[A]]): M[Seq[Query[Json[A]]]] =
    v match {
      case Query.ValidQuery(path, Json.Array(elements, _)) =>
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
    }
}

inline given queryToSequenceIdConversion[A](
      using mcb: ConvertibleBy[({type Id[T] = T})#Id, A])
    : Conversion[Query[Json[A]], Seq[Query[Json[A]]]] =
  queryToSequenceConversion


/**
 * Conversion from Query to map with queries as values.
 */
given queryToMapConversion[M[_], A](using mcb: ConvertibleBy[M, A])
    : Conversion[Query[Json[A]], M[Map[String, Query[Json[A]]]]] with {
  override def apply(v: Query[Json[A]]): M[Map[String, Query[Json[A]]]] =
    v match {
      case Query.ValidQuery(path, Json.Object(elements, _)) =>
        val content =
          for
            (key, value) <- elements
          yield
            (key -> Query.ValidQuery(path / key, value.value))
        mcb.pure(content.toMap)
      case Query.ValidQuery(path, nonArray) =>
        mcb.invalidDomainValue(path, nonArray, "Object expected")
      case Query.MissingElement(validPath, value, invalidPath) =>
        mcb.fieldMissing(validPath, value, invalidPath)
      case Query.InvalidSelector(validPath, value, invalidPath) =>
        mcb.accessError(validPath, value, invalidPath)
    }
}


inline given queryToMapIdConversion[A](
      using mcb: ConvertibleBy[({type Id[T] = T})#Id, A])
    : Conversion[Query[Json[A]], Map[String, Query[Json[A]]]] =
  queryToMapConversion
