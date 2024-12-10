package io.github.maxkar
package json.attr

import json.query.Path

/**
 * Integration with conversion (and error) typeclass.
 * The typeclass may encode conversions in some specific ways as needed.
 * @tparam M conversion target (monad).
 * @tparam A type of json Attributes.
 */
trait ConvertibleBy[M[_], A] {
  /** Wraps a "successfull" value. */
  def pure[T](v: T): M[T]

  /** Encodes field access error. */
  def accessError[T](validPath: Path, value: Json[A], invalidPath: Path): M[T]

  /** Encodes access of an absent field. */
  def fieldMissing[T](validPath: Path, value: Json[A], invalidPath: Path): M[T]

  /**
   * Encodes access to value that seems to be valid according to JSON definition
   * but is not valid for the target domain (number too large, string is not valid
   * date, etc...)
   */
  def invalidDomainValue[T](path: Path, value: Json[A], message: String): M[T]
}


object ConvertibleBy {
  /** Identity "monad" type. */
  type Identity[T] = T

  /**
   * Creates a convertible implementation for identity type constructor.
   * The implementation * throws IOException on errors.
   *
   * @tparam A type of attributes on the JSON model.
   * @param location function used to exctract location from the attributes.
   */
  def identityWithLocation[A](location: A => String): ConvertibleBy[Identity, A] =
    new ConvertibleBy[Identity, A] {
      import java.io.IOException

      override def pure[T](v: T): Identity[T] = v

      override def accessError[T](validPath: Path, value: Json[A], invalidPath: Path): Identity[T] =
        throw new IOException(
          s"${location(value.attrs)} (${validPath}): Value of type ${Json.typeName(value)} could not be navigate using the path ${invalidPath}"
        )

      override def fieldMissing[T](validPath: Path, value: Json[A], invalidPath: Path): Identity[T] =
        throw new IOException(
          s"${location(value.attrs)} (${validPath}): Value does not have an element that matches the path ${invalidPath}"
        )

      override def invalidDomainValue[T](path: Path, value: Json[A], message: String): Identity[T] =
        throw new IOException(
          s"${location(value.attrs)} (${path}): Could not convert JSON value into domain value: ${message}"
        )
    }
}
