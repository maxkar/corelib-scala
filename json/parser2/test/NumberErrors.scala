package io.github.maxkar
package json.parser

/** Implementation of number errors reused across tests. */
given NumberErrors: Numbers.Errors[Identity] with
  /** Encoding for missing integer digits. */
  case class MissingIntegerDigits() extends Exception

  /** Encoding for missing decimal digits. */
  case class MissingDecimalDigits() extends Exception

  /** Encoding for missing exponent digits. */
  case class MissingExponentDigits() extends Exception

  /** Encoding for integer with leading 0. */
  case class LeadingIntegerZero() extends Exception

  override def missingIntegerDigits[T](): T =
    throw new MissingIntegerDigits()

  override def leadingIntegerZero[T](): T =
    throw new LeadingIntegerZero()

  override def missingDecimalDigits[T](): T =
    throw new MissingDecimalDigits()

  override def missingExponentDigits[T](): T =
    throw new MissingExponentDigits()
end NumberErrors
