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
