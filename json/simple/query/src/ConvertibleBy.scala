package io.github.maxkar
package json.simple.query

import json.simple.Json

import json.query.Path

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

  /**
   * Convertible implementation for identity type constructor. The implementation
   * throws IOException on errors.
   *
   * It is supposed to be imported like
   * {{{
   *   import io.github.maxkar.json.simple.query.ConvertibleBy.Identity.given
   * }}}
   */
  object Identity:
    /** "Monad" type. */
    type M[T] = T

    /** Implementation of given in "general" execution. */
    given identityConvertible: ConvertibleBy[M] with
      import java.io.IOException

      override def pure[T](v: T): M[T] = v

      override def accessError[T](validPath: Path, value: Json, invalidPath: Path): M[T] =
        throw new IOException(
          s"${validPath}: Value of type ${Json.typeName(value)} could not be navigate using the path ${invalidPath}"
        )

      override def fieldMissing[T](validPath: Path, value: Json, invalidPath: Path): M[T] =
        throw new IOException(
          s"${validPath}: Value does not have an element that matches the path ${invalidPath}"
        )

      override def invalidDomainValue[T](path: Path, value: Json, message: String): M[T] =
        throw new IOException(
          s"${path}: Could not convert JSON value into domain value: ${message}"
        )
    end identityConvertible
  end Identity
end ConvertibleBy
