package io.github.maxkar
package json.simple

import json.simple.Json

import json.query.Path

import fun.instances.Identity
import fun.typeclass.Applicative


/**
 * Integration with conversion (and error) typeclass.
 * The typeclass may encode conversions in some specific ways as needed.
 * @tparam M conversion target (monad).
 */
trait ConvertibleBy[M[_]]:
  /** Wraps a "successfull" value. */
  def pure[T](v: T): M[T]

  /** Encodes field access error. */
  def accessError[T](validPath: Path, value: Json, invalidPath: Path): M[T]

  /** Encodes access of an absent field. */
  def fieldMissing[T](validPath: Path, value: Json, invalidPath: Path): M[T]

  /**
   * Encodes access to value that seems to be valid according to JSON definition
   * but is not valid for the target domain (number too large, string is not valid
   * date, etc...)
   */
  def invalidDomainValue[T](path: Path, value: Json, message: String): M[T]
end ConvertibleBy


object ConvertibleBy:

  /** Simple error converters - it knows how to "raise" error given the message. */
  trait SimpleErrors[M[_]]:
    /** "Raises" the issue with the given message. */
    def raise[T](message: String): M[T]
  end SimpleErrors


  object SimpleErrors:
    /** An implementation that just raises IOException on the calling thread. */
    def raiseIOException[M[_]]: SimpleErrors[M] =
      new SimpleErrors[M] {
        override def raise[T](message: String): M[T] =
          throw new java.io.IOException(message)
      }
  end SimpleErrors


  /**
   * Creates a new converter for the given execution with the provide
   * error reporter.
   */
  def apply[M[_]: Applicative](errors: SimpleErrors[M]): ConvertibleBy[M] =
    new ConvertibleBy[M] {
      override def pure[T](v: T): M[T] = Applicative.pure(v)

      override def accessError[T](validPath: Path, value: Json, invalidPath: Path): M[T] =
        errors.raise(
          s"${validPath}: Value of type ${Json.typeName(value)} could not be navigate using the path ${invalidPath}"
        )

      override def fieldMissing[T](validPath: Path, value: Json, invalidPath: Path): M[T] =
        errors.raise(
          s"${validPath}: Value does not have an element that matches the path ${invalidPath}"
        )

      override def invalidDomainValue[T](path: Path, value: Json, message: String): M[T] =
        errors.raise(
          s"${path}: Could not convert JSON value into domain value: ${message}"
        )
    }
  end apply
end ConvertibleBy
