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
 * ## Continuation-based parsing
 *
 * The client starts parsing by invoking `startParsing` and receives porting of the input and the next
 * state. This method may look a bit tricky to use. However it has an advantage of being able to parse
 * complete number in ONE go with sufficiently populated buffer thus avoid unnecessary copying required
 * to construct the full number representation on the client side (in intermediate StringBuilder or any
 * other class with the similar functionality).
 *
 * Usage pseudocode:
 * ```
 * def parseNumber() =
 *   var (chars, nextState) = startParsing(input)
 *   if (nextState == null)
 *     return chars.toString()
 *   val buf = new StringBuilder(chars)
 *   while nextState != null do
 *     (chars, nextState) = nextState.continue(input)
 *     buf ++= chars
 *   end while
 *   return buf.toString()
 * end parseNumber
 * ```
 *
 * ## Pull-based "Iterator" parsing
 *
 * The client has an iterator by invoking `newReader()` and then invokes `next()` until receives null.
 *
 * Usage pseudocode:
 * ```
 * val reader = Numbers.newReader(input)
 *
 * val buf = new StringBuilder()
 * var rd = reader.next()
 * while rd != null do
 *   buf ++= rd
 *   rd = reader.next()
 * val number = buf.toString()
 * ```
 *
 * ## Wholistic Reading
 *
 * The number representation could be read completely in one call to `readAll`.
 */
object Numbers:
  /**
   * Errors that may occur during number parsing.
   */
  trait Errors[M[_]]:
    /** Indicates that the integer part of the number is missing. */
    def missingIntegerDigits[T](): M[T]

    /** Indicates that leading zero is present in the integer section. */
    def leadingIntegerZero[T](): M[T]

    /** Indicates that fractional (decimal) part does not have any digits. */
    def missingDecimalDigits[T](): M[T]

    /** Indicates that exponent part does not have any digits. */
    def missingExponentDigits[T](): M[T]
  end Errors


  /**
   * Description of how to **continue** parsing of the specific element.
   * The continuation captures the state of the parsing and is able to properly
   * resume parsing when demanded.
   */
  abstract sealed class ParsingContinuation private[Numbers]():
    /**
     * Continues parsing of the input (from the given state).
     * @param stream stream containing data to parse.
     * @return pair of consumed input and the **nullable** next continuation to use. The sequence is
     *   never `null` (but may be an empty sequence). The continuation may be `null`, this value indicates
     *   that the number was parsed completely.
     */
    def continue[M[_]: Monad: Errors](stream: CharacterStream[M]): M[(CharSequence, ParsingContinuation)]
  end ParsingContinuation


  /** Checks if the character is valid number sign. */
  def isNumberSign(char: Char): Boolean =
    char == '-'

  /** Checks if the character is a decimal separator. */
  def isDecimalSeparator(char: Char): Boolean =
    char == '.'

  /** Checks if the character is an exponent indicator. */
  def isExponentIndicator(char: Char): Boolean =
    char == 'E' || char == 'e'

  /** Checks if the character is an exponent sign. */
  def isExponentSign(char: Char): Boolean =
    char == '+' || char == '-'

  /**
   * Checks if the character is a valid (json) digit. The character being digit
   * does not mean that the sequence of characters would be valid number. The
   * integer value part (and the only part) could not have leading zero(es).
   */
  def isDigit(char: Char): Boolean =
    '0' <= char && char <= '9'

  /**
   * Checks if the character is valid number start.
   */
  def isNumberStart(char: Char): Boolean =
    isNumberSign(char) || isDigit(char)

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
   * Starts number parsing. May return optional next state.
   * @param stream stream to read the number from.
   * @return pair consisting of the consumed portion of the number (never `null`) and
   *   optional (nullable) parser that should be used to consume next portion of the number.
   */
  def startParsing[M[_]: Monad: Errors](stream: CharacterStream[M]): M[(CharSequence, ParsingContinuation)] =
    NumberParser.continue(stream)


  /** Creates new iterator-like pull number reader. */
  def newReader[M[_]: Monad: Errors](stream: CharacterStream[M]): NumberReader[M] =
    new NumberReader(NumberParser, stream)


  /** Reads the number fully. This may be memory-inefficient for huge numbers. */
  def readAll[M[_]: Monad: Errors](stream: CharacterStream[M]): M[String] =
    startParsing(stream) flatMap { (inputPortion, nextState) =>
      /* Check if we could go happy-path (all number contents is available) or not. */
      if nextState == null then
        Monad.pure(inputPortion.toString())
      else
        val buffer = new StringBuilder()
        buffer.append(inputPortion)
        readAllImpl(stream, buffer, nextState)
    }
  end readAll


  /** Internal "accumulating" implementation of read-all. */
  private def readAllImpl[M[_]: Monad: Errors](
          stream: CharacterStream[M],
          buffer: StringBuilder,
          state: ParsingContinuation
      ): M[String] =
    state.continue(stream) flatMap { (inputPortion, nextState) =>
      buffer.append(inputPortion)
      if nextState == null then
        Monad.pure(buffer.toString)
      else
        readAllImpl(stream, buffer, nextState)
    }


  /** Parser of the state **inside** exponent digits. */
  private object ExponentDigitsParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(1) flatMap { lookAhead =>
        if lookAhead.length() > 0 then
          continueFrom(stream, lookAhead, 0)
        else
          Monad.pure(("", null))
      }
    end continue


    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: CharacterStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] =
      val nonDigit = Numbers.skipDigits(lookAhead, offset)
      val nextState =
        if nonDigit >= lookAhead.length() then
          ExponentDigitsParser
        else
          null: ParsingContinuation
      stream.consume(nonDigit) map (chars => (chars, nextState))
    end continueFrom
  end ExponentDigitsParser


  /** Parser that reports missing exponent digits. */
  private object MissingExponentDigitsParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      implicitly[Errors[M]].missingExponentDigits()
  end MissingExponentDigitsParser


  /** Parser for the optional exponent part. */
  private object MaybeExponentParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(3) flatMap { lookAhead =>
        /* EOF and non-exponent condition first. */
        if lookAhead.length() <= 0 || !Numbers.isExponentIndicator(lookAhead.charAt(0)) then
          Monad.pure(("", null))
        else
          var ptr = 1
          /* Consume optional exponent sign. */
          if ptr < lookAhead.length() && Numbers.isExponentSign(lookAhead.charAt(ptr)) then
            ptr += 1

          if ptr < lookAhead.length() && Numbers.isDigit(lookAhead.charAt(ptr)) then
            ExponentDigitsParser.continueFrom(stream, lookAhead, ptr)
          else
            stream.consume(ptr) map { chars => (chars, MissingExponentDigitsParser)}
        end if
      }
    end continue


    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: CharacterStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] =

      /* No exponent part at all (assuming we _have_ some data to look at). */
      if !Numbers.isExponentIndicator(lookAhead.charAt(offset)) then
        return stream.consume(offset) map { chars => (chars, null) }

      var ptr = offset + 1
      if ptr < lookAhead.length() && Numbers.isExponentSign(lookAhead.charAt(ptr)) then
        ptr += 1

      /* Start looks like valid exponent but we don't know if more digits will follow.
       * We are in the context where we can postpone reading/consuming some characters so
       * no need to introduce extra states, just defer reading _full_ exponent (including
       * indicator) for later.
       */
      if ptr >= lookAhead.length() then
        return stream.consume(offset) map { chars => (chars, MaybeExponentParser) }

      /* Now we have enough data to see if it is valid number or not. Proceed with
       * either error (but consume exponent dot and sign to report error at the correct
       * position) or delegate to digit consumer.
       */
      if !Numbers.isDigit(lookAhead.charAt(ptr)) then
        stream.consume(ptr) map { chars => (chars, MissingExponentDigitsParser) }
      else
        ExponentDigitsParser.continueFrom(stream, lookAhead, ptr)
    end continueFrom
  end MaybeExponentParser


  /** Parser for decimal digits and the rest of the input. */
  private object DecimalDigitsParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(3) flatMap { lookAhead =>
        if lookAhead.length() <= 0 then
          Monad.pure(("", null))
        else
          continueFrom(stream, lookAhead, 0)
      }
    end continue

    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: CharacterStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] =
      val nonDigit = Numbers.skipDigits(lookAhead, offset)

      if nonDigit >= lookAhead.length() then
        stream.consume(nonDigit) map { chars => (chars, DecimalDigitsParser)}
      else
        MaybeExponentParser.continueFrom(stream, lookAhead, nonDigit)
    end continueFrom
  end DecimalDigitsParser


  /** Parser that reports missing decimal digits. */
  private object MissingDecimalDigitsParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      implicitly[Errors[M]].missingDecimalDigits()
  end MissingDecimalDigitsParser


  private object MaybeDecimalPartParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      /* Request 3 just in case there is just exponent but no decimal part. This way
       * there are no retruns of empty sequence (and no 0-length consume calls).
       */
      stream.peek(3) flatMap { lookAhead =>
        if lookAhead.length <= 0 then
          Monad.pure(("", null))
        else if !Numbers.isDecimalSeparator(lookAhead.charAt(0)) then
          MaybeExponentParser.continueFrom(stream, lookAhead, 0)
        else if lookAhead.length() <= 1 || !Numbers.isDigit(lookAhead.charAt(1)) then
          stream.consume(1) map { chars => (chars, MissingDecimalDigitsParser) }
        else
          DecimalDigitsParser.continueFrom(stream, lookAhead, 2)
      }
    end continue

    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: CharacterStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] =
      if !Numbers.isDecimalSeparator(lookAhead.charAt(offset)) then
        return MaybeExponentParser.continueFrom(stream, lookAhead, offset)

      val ptr = offset + 1
      if ptr >= lookAhead.length() then
        /* Do not consume dot, consume it on the next iteration of the parsing. */
        stream.consume(offset) map { chars => (chars, MaybeDecimalPartParser) }
      else if Numbers.isDigit(lookAhead.charAt(ptr)) then
        DecimalDigitsParser.continueFrom(stream, lookAhead, ptr)
      else
        stream.consume(ptr) map { chars => (chars, MissingDecimalDigitsParser) }
    end continueFrom
  end MaybeDecimalPartParser


  /** Parser for decimal digits and the rest of the input. */
  private object IntegerDigitsParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(3) flatMap { lookAhead =>
        if lookAhead.length() <= 0 then
          Monad.pure(("", null))
        else
          continueFrom(stream, lookAhead, 0)
      }
    end continue


    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: CharacterStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] =
      val nonDigit = Numbers.skipDigits(lookAhead, offset)

      if nonDigit >= lookAhead.length() then
        stream.consume(nonDigit) map { chars => (chars, IntegerDigitsParser)}
      else
        MaybeDecimalPartParser.continueFrom(stream, lookAhead, nonDigit)
    end continueFrom
  end IntegerDigitsParser


  /** Parser that reports missing integer digits. */
  private object MissingIntegerDigitsParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      implicitly[Errors[M]].missingIntegerDigits()
  end MissingIntegerDigitsParser



  /** Parser that reports leading 0 in the integer part. */
  private object Leading0Parser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      implicitly[Errors[M]].leadingIntegerZero()
  end Leading0Parser


  /** Parser for the numbers. */
  private object NumberParser extends ParsingContinuation:
    override def continue[M[_]: Monad: Errors](
          stream: CharacterStream[M]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(3) flatMap { lookAhead =>
        val hasSign = lookAhead.length() > 0 && Numbers.isNumberSign(lookAhead.charAt(0))
        val firstDigitIndex = if hasSign then 1 else 0
        val secondDigitIndex = firstDigitIndex + 1

        if lookAhead.length() <= firstDigitIndex || !Numbers.isDigit(lookAhead.charAt(firstDigitIndex)) then
          if hasSign then
            stream.consume(1) map { chars => (chars, MissingIntegerDigitsParser) }
          else
            implicitly[Errors[M]].missingIntegerDigits()
        else if lookAhead.charAt(firstDigitIndex) == '0'
            && secondDigitIndex < lookAhead.length()
            && Numbers.isDigit(lookAhead.charAt(secondDigitIndex)) then
          if hasSign then
            stream.consume(1) map { chars => (chars, Leading0Parser) }
          else
            implicitly[Errors[M]].leadingIntegerZero()
        else
          IntegerDigitsParser.continueFrom(stream, lookAhead, firstDigitIndex)
      }
    end continue
  end NumberParser
end Numbers
