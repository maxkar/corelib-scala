package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Monad
import fun.Applicative

/**
 * Number-related functionality.
 *
 * Provides multiple utilities and predicates that may be used by clients
 * of different levels to implement number reading functionality.
 *
 * ## Predicates
 *
 * The family of isXXX methods (`isNumberSign`, `isDecimalSeparator`,
 * `isExponentIndicator`, `isExponentSign`, `isDigit`) may be used by implementations
 * doing their own state management
 *
 * ## Partial Reads
 *
 * The `readXXX` sections may be used by clients reading number "section by section" (i.e.
 * internally separating digits, exponent, etc...).
 * Number sequences should be parsed in the following way:
 *
 * ```
 *   var buf = readIntegerDigitsStart() // or other readXXXStart
 *   var next = readNextDigits()
 *   while next != null do
 *     buf += next
 *     next = readNextDigits()
 * ```
 *
 * ## State-assisted parsing
 *
 * The client manages all input/output and deals with number representation. The utility helps the client
 * in identifying number boundaries and potential errors in the number representation. The parsing
 * is initiated by the `scanner()` call and is described by the `NumberScanner` class.
 */
object Numbers:
  /** Synonym for the NumberScanner type for convenient access. */
  type Scanner = NumberScanner
  /** Synonym for the NumberScanner companion for convenient access. */
  val Scanner = NumberScanner

  /**
   * Errors that may occur during number parsing.
   */
  trait Errors[M[_]]:
    /** Indicates that the integer part of the number is missing. */
    def missingInteregPart[T](): M[T]

    /** Indicates that leading zero is present in the integer section. */
    def leadingIntegerZero[T](): M[T]

    /** Indicates that fractional (decimal) part does not have any digits. */
    def missingDecimalDigits[T](): M[T]

    /** Indicates that exponent part does not have any digits. */
    def missingExponentDigits[T](): M[T]
  end Errors



  /** Checks if the character is valid number sign. */
  def isNumberSign(char: Character): Boolean =
    char == '-'

  /** Checks if the character is a decimal separator. */
  def isDecimalSeparator(char: Character): Boolean =
    char == '.'

  /** Checks if the character is an exponent indicator. */
  def isExponentIndicator(char: Character): Boolean =
    char == 'E' || char == 'e'

  /** Checks if the character is an exponent sign. */
  def isExponentSign(char: Character): Boolean =
    char == '+' || char == '-'

  /**
   * Checks if the character is a valid (json) digit. The character being digit
   * does not mean that the sequence of characters would be valid number. The
   * integer value part (and the only part) could not have leading zero(es).
   */
  def isDigit(char: Character): Boolean =
    '0' <= char && char <= '9'

  /**
   * Counts number of digits in the prefix.
   */
  def countDigits(buf: CharSequence): Int =
    var res = 0
    while res < buf.length() && isDigit(buf.charAt(res)) do
      res += 1
    res
  end countDigits


  /**
   * "Skips" digits from the given position in the input and returns a "non-digit" position.
   * @param chars characters to go over.
   * @param start first character to look at.
   * @return index of the first non-digit character at or after the `start` position or
   *   `chars.length()` if all characters starting from `start` are digits.
   */
  def skipDigits(chars: CharSequence, start: Int): Int =
    var ptr = start
    while ptr < chars.length() && Numbers.isDigit(chars.charAt(ptr)) do
      ptr += 1
    ptr
  end skipDigits


  /**
   * Reads an optional number sign from the stream.
   * @param stream stream to read from.
   * @return optional sign character that occurs in the number.
   */
  def readOptionalNumberSign[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Option[Character]] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isNumberSign(lookAhead.charAt(0)) then
        stream.skip(1) map { rd => Some('-') }
      else
        Monad.pure(None)
    }
  end readOptionalNumberSign


  /**
   * Reads an optional number sign. Returns "absent" value as 0.
   * This is similar * to the
   * ```
   *   for
   *     maybeCode <- readOptionalNumberSign(stream)
   *   yield maybeCode.getOrElse(0)
   * ```
   * but may be more efficient.
   */
  def readNumberSignAsOptChar[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Character] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isNumberSign(lookAhead.charAt(0)) then
        stream.skip(1) map { rd => '-' }
      else
        Monad.pure(0: Char)
    }
  end readNumberSignAsOptChar


  /**
   * Reads initial section of the integer part of the number (without sign). May
   * raise errors if number is mailformed.
   */
  def readIntegerDigitsStart[M[_]: Monad: Errors](
        stream: CharacterStream[M]
      ) : M[CharSequence] =
    stream.peek(2) flatMap { lookAhead =>
      val totalDigits = countDigits(lookAhead)
      if totalDigits < 1 then
        implicitly[Errors[M]].missingInteregPart()
      else if totalDigits > 1 && lookAhead.charAt(0) == '0' then
        implicitly[Errors[M]].leadingIntegerZero()
      else
        stream.consume(totalDigits)
    }
  end readIntegerDigitsStart


  /**
   * Reads an optional indicator of the fractional part.
   */
  def readOptionalDecimalSeparator[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Option[Char]] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isDecimalSeparator(lookAhead.charAt(0)) then
        stream.skip(1) map { rd => Some('.') }
      else
        Monad.pure(None)
    }
  end readOptionalDecimalSeparator


  /**
   * Reads an optional indicator of the fractional part representing absent
   * value as a character 0.
   *
   * This is similar * to the
   * ```
   *   for
   *     maybeCode <- readOptionalDecimalSeparator(stream)
   *   yield maybeCode.getOrElse(0)
   * ```
   * but may be more efficient.
   */
  def readDecimalSeparatorAsOptChar[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Char] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isDecimalSeparator(lookAhead.charAt(0)) then
        stream.skip(1) map { rd => '.' }
      else
        Monad.pure(0)
    }
  end readDecimalSeparatorAsOptChar


  /**
   * Reads an optional indicator of the fractional part and indicates if the
   * part was read (`true`) or not (`false`).
   */
  def readDecimalSeparatorAsBoolean[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Boolean] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isDecimalSeparator(lookAhead.charAt(0)) then
        stream.skip(1) map { _ => true }
      else
        Monad.pure(false)
    }
  end readDecimalSeparatorAsBoolean


  /**
   * Reads initial section of the fractional (decimal) part of the number. May  raise errors if
   * number is mailformed.
   */
  def readDecimalDigitsStart[M[_]: Monad: Errors](
        stream: CharacterStream[M]
      ) : M[CharSequence] =
    stream.peek(1) flatMap { lookAhead =>
      val totalDigits = countDigits(lookAhead)
      if totalDigits < 1 then
        implicitly[Errors[M]].missingDecimalDigits()
      else
        stream.consume(totalDigits)
    }
  end readDecimalDigitsStart


  /**
   * Reads an optional indicator of the exponential part.
   */
  def readOptionalExponentSeparator[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Option[Char]] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isExponentIndicator(lookAhead.charAt(0)) then
        stream.consume(1) map { rd => Some(rd.charAt(0)) }
      else
        Monad.pure(None)
    }
  end readOptionalExponentSeparator


  /**
   * Reads an optional exponent indicator representing absent value as a character 0.
   *
   * This is similar * to the
   * ```
   *   for
   *     maybeCode <- readOptionalExponentIndicator(stream)
   *   yield maybeCode.getOrElse(0)
   * ```
   * but may be more efficient.
   */
  def readExponentIndicatorAsOptChar[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Char] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isExponentIndicator(lookAhead.charAt(0)) then
        stream.skip(1) map { rd => '.' }
      else
        Monad.pure(0)
    }
  end readExponentIndicatorAsOptChar


  /**
   * Reads an optional exponent indicator and indicates if the
   * part was read (`true`) or not (`false`).
   */
  def readExponentIndicatorAsBoolean[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Boolean] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isExponentIndicator(lookAhead.charAt(0)) then
        stream.skip(1) map { _ => true }
      else
        Monad.pure(false)
    }
  end readExponentIndicatorAsBoolean


  /**
   * Reads an optional exponent  sign from the stream.
   * @param stream stream to read from.
   * @return optional exponent sign character that occurs in the number.
   */
  def readOptionalExponentSign[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Option[Character]] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isExponentSign(lookAhead.charAt(0)) then
        stream.consume(1) map { rd => Some(rd.charAt(0)) }
      else
        Monad.pure(None)
    }
  end readOptionalExponentSign


  /**
   * Reads an optional exponent sign. Returns "absent" value as 0.
   * This is similar * to the
   * ```
   *   for
   *     maybeCode <- readOptionalExponentSign(stream)
   *   yield maybeCode.getOrElse(0)
   * ```
   * but may be more efficient.
   */
  def readExponentSignAsOptChar[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Character] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isExponentSign(lookAhead.charAt(0)) then
        stream.consume(1) map { rd => rd.charAt(0) }
      else
        Monad.pure(0: Char)
    }
  end readExponentSignAsOptChar


  /**
   * Reads initial section of the exponent. May  raise errors if number is mailformed.
   */
  def readExponentDigitsStart[M[_]: Monad: Errors](
        stream: CharacterStream[M]
      ) : M[CharSequence] =
    stream.peek(1) flatMap { lookAhead =>
      val totalDigits = countDigits(lookAhead)
      if totalDigits < 1 then
        implicitly[Errors[M]].missingExponentDigits()
      else
        stream.consume(totalDigits)
    }
  end readExponentDigitsStart


  /**
   * Continues reading digits after intial part was consumed by the
   * specific `readXXXDigitsStart` method.
   * This method return `null` to indicate that there are no more digits
   * that could be read in the stream.
   */
  def readNextDigits[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[CharSequence] =
    stream.peek(1) flatMap { lookAhead =>
      val totalDigits = countDigits(lookAhead)
      if totalDigits <= 0 then
        Monad.pure(null)
      else
        stream.consume(totalDigits)
    }
  end readNextDigits


  /**
   * Creates a new number scanner to assist the client in marking number boundaries
   * in the input.
   */
  def scanner(): Scanner = new NumberScanner()

end Numbers
