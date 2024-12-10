package io.github.maxkar
package json.parser

import fun.typeclass.Monad
import fun.typeclass.Applicative

import text.input.LookAheadStream

/** Number-related parsing functionality. */
object Numbers {
  /**
   * Error handlers for JSON numbers.
   * @tparam M execution (monad).
   * @tparam S type of the stream (i.e. "context") required by the error generation.
   */
  trait Errors[M[_], -S] {
    /**
     * Invoked when integer digits are missing in the number.
     * Stream position is before the character that should be an integer digit but
     * is not. The position is after the number sign (if present).
     */
    def missingIntegerDigits[T](stream: S): M[T]

    /**
     * Invoked when a number's integer part contains leading zero (and the part is
     * not just a simple zero).
     * Stream position is before the leading zero.
     */
    def leadingIntegerZero[T](stream: S): M[T]

    /**
     * Invoked when the number has decimal separator but no decimal digits.
     * Stream position is after the decimal separator (i.e. just before where
     * a first decimal digit should occur).
     */
    def missingDecimalDigits[T](stream: S): M[T]

    /**
     * Invoked when the number has exponent indicator but no exponent digits.
     * Stream position is just before where the first exponent digit was expected.
     * This is after the exponent indicator and an optional exponent sign.
     */
    def missingExponentDigits[T](stream: S): M[T]
  }


  /**
   * Description of how to **continue** parsing of the specific element.
   * The continuation captures the state of the parsing and is able to properly
   * resume parsing when demanded.
   */
  abstract sealed class ParsingContinuation private[Numbers]() {
    /**
     * Continues parsing of the input (from the given state).
     * @param stream stream containing data to parse.
     * @return pair of consumed input and the **nullable** next continuation to use. The sequence is
     *   never `null` (but may be an empty sequence). The continuation may be `null`, this value indicates
     *   that the number was parsed completely.
     */
    def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)]
  }


  /**
   * "Iterator-like" reader of number. Reads number section by section
   * and returns `null` when there are no more parts to read.
   */
  abstract sealed class Reader[M[_]] private[Numbers]() {
    /** Reads next part of the number. Returns `null` (within monad) if all the number was read. */
    def next(): M[CharSequence]
  }


  /**
   * Implementation of the reader. This one has extra type parameters not
   * exposed by the (provided) `Reader` API.
   */
  private final class ReaderImpl[M[_]: Monad, S <: LookAheadStream[M]] private[Numbers](
        private var state: Numbers.ParsingContinuation,
        stream: S,
      )(using
        errs: Numbers.Errors[M, S]
      )
      extends Reader[M] {

    override def next(): M[CharSequence] = {
      if state == null then
        Monad.pure(null)
      else
        state.continue(stream) map { (chars, newState) =>
          state = newState
          chars
        }
    }
  }


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
  def countDigits(buf: CharSequence): Int = {
    var res = 0
    while res < buf.length() && isDigit(buf.charAt(res)) do
      res += 1
    res
  }

  /**
   * "Skips" digits from the given position in the input and returns a "non-digit" position.
   * @param chars characters to go over.
   * @param start first character to look at.
   * @return index of the first non-digit character at or after the `start` position or
   *   `chars.length()` if all characters starting from `start` are digits.
   */
  def skipDigits(chars: CharSequence, start: Int): Int = {
    var ptr = start
    while ptr < chars.length() && Numbers.isDigit(chars.charAt(ptr)) do
      ptr += 1
    ptr
  }


  /**
   * Starts number parsing. May return optional next state.
   * @param stream stream to read the number from.
   * @return pair consisting of the consumed portion of the number (never `null`) and
   *   optional (nullable) parser that should be used to consume next portion of the number.
   */
  def startParsing[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S
      )(using
        errs: Errors[M, S]
      ): M[(CharSequence, ParsingContinuation)] =
    NumberParser.continue(stream)


  /** Creates new iterator-like pull number reader. */
  def newReader[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): Reader[M] =
    new ReaderImpl(NumberParser, stream)


  /** Reads the number fully. This may be memory-inefficient for huge numbers. */
  def readAll[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S,
      )(using
        errs: Errors[M, S]
      ): M[String] =
    startParsing(stream) flatMap { (inputPortion, nextState) =>
      /* Check if we could go happy-path (all number contents is available) or not. */
      if nextState == null then
        Monad.pure(inputPortion.toString())
      else
        val buffer = new StringBuilder()
        buffer.append(inputPortion)
        readAllImpl(stream, buffer, nextState)
    }


  /** Internal "accumulating" implementation of read-all. */
  private def readAllImpl[M[_]: Monad, S <: LookAheadStream[M]](
        stream: S,
        buffer: StringBuilder,
        state: ParsingContinuation,
      )(using
        errs: Errors[M, S]
      ): M[String] =
    state.continue(stream) flatMap { (inputPortion, nextState) =>
      buffer.append(inputPortion)
      if nextState == null then
        Monad.pure(buffer.toString)
      else
        readAllImpl(stream, buffer, nextState)
    }


  /** Parser of the state **inside** exponent digits. */
  private object ExponentDigitsParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(1) flatMap { lookAhead =>
        if lookAhead.length() > 0 then
          continueFrom(stream, lookAhead, 0)
        else
          Monad.pure(("", null))
      }


    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: LookAheadStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] = {
      val nonDigit = Numbers.skipDigits(lookAhead, offset)
      val nextState =
        if nonDigit >= lookAhead.length() then
          ExponentDigitsParser
        else
          null: ParsingContinuation
      stream.consume(nonDigit) map (chars => (chars, nextState))
    }
  }


  /** Parser that reports missing exponent digits. */
  private object MissingExponentDigitsParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      errs.missingExponentDigits(stream)
  }


  /** Parser for the optional exponent part. */
  private object MaybeExponentParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(3) flatMap { lookAhead =>
        /* EOF and non-exponent condition first. */
        if lookAhead.length() <= 0 || !Numbers.isExponentIndicator(lookAhead.charAt(0)) then
          Monad.pure(("", null))
        else {
          var ptr = 1
          /* Consume optional exponent sign. */
          if ptr < lookAhead.length() && Numbers.isExponentSign(lookAhead.charAt(ptr)) then
            ptr += 1

          if ptr < lookAhead.length() && Numbers.isDigit(lookAhead.charAt(ptr)) then
            ExponentDigitsParser.continueFrom(stream, lookAhead, ptr)
          else
            stream.consume(ptr) map { chars => (chars, MissingExponentDigitsParser)}
        }
      }


    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: LookAheadStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] = {

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
    }
  }


  /** Parser for decimal digits and the rest of the input. */
  private object DecimalDigitsParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(3) flatMap { lookAhead =>
        if lookAhead.length() <= 0 then
          Monad.pure(("", null))
        else
          continueFrom(stream, lookAhead, 0)
      }

    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] = {
      val nonDigit = Numbers.skipDigits(lookAhead, offset)

      if nonDigit >= lookAhead.length() then
        stream.consume(nonDigit) map { chars => (chars, DecimalDigitsParser)}
      else
        MaybeExponentParser.continueFrom(stream, lookAhead, nonDigit)
    }
  }


  /** Parser that reports missing decimal digits. */
  private object MissingDecimalDigitsParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      errs.missingDecimalDigits(stream)
  }


  private object MaybeDecimalPartParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
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


    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: LookAheadStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] = {
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
    }
  }


  /** Parser for decimal digits and the rest of the input. */
  private object IntegerDigitsParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      stream.peek(3) flatMap { lookAhead =>
        if lookAhead.length() <= 0 then
          Monad.pure(("", null))
        else
          continueFrom(stream, lookAhead, 0)
      }


    /**
     * Continues parsing from the given position in the look-ahead stream.
     * This may be used from other parsers to consume more parts of the already available data.
     */
    private[Numbers] def continueFrom[M[_]: Monad](
          stream: LookAheadStream[M],
          lookAhead: CharSequence,
          offset: Int = 0
        ): M[(CharSequence, ParsingContinuation)] = {
      val nonDigit = Numbers.skipDigits(lookAhead, offset)

      if nonDigit >= lookAhead.length() then
        stream.consume(nonDigit) map { chars => (chars, IntegerDigitsParser)}
      else
        MaybeDecimalPartParser.continueFrom(stream, lookAhead, nonDigit)
    }
  }


  /** Parser that reports missing integer digits. */
  private object MissingIntegerDigitsParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      errs.missingIntegerDigits(stream)
  }


  /** Parser that reports leading 0 in the integer part. */
  private object Leading0Parser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] =
      errs.leadingIntegerZero(stream)
  }


  /** Parser for the numbers. */
  private object NumberParser extends ParsingContinuation {
    override def continue[M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
        )(using
          errs: Errors[M, S]
        ): M[(CharSequence, ParsingContinuation)] = {
      stream.peek(3) flatMap { lookAhead =>
        val hasSign = lookAhead.length() > 0 && Numbers.isNumberSign(lookAhead.charAt(0))
        val firstDigitIndex = if hasSign then 1 else 0
        val secondDigitIndex = firstDigitIndex + 1

        if lookAhead.length() <= firstDigitIndex || !Numbers.isDigit(lookAhead.charAt(firstDigitIndex)) then
          if hasSign then
            stream.consume(1) map { chars => (chars, MissingIntegerDigitsParser) }
          else
            errs.missingIntegerDigits(stream)
        else if lookAhead.charAt(firstDigitIndex) == '0'
            && secondDigitIndex < lookAhead.length()
            && Numbers.isDigit(lookAhead.charAt(secondDigitIndex)) then
          if hasSign then
            stream.consume(1) map { chars => (chars, Leading0Parser) }
          else
            errs.leadingIntegerZero(stream)
        else
          IntegerDigitsParser.continueFrom(stream, lookAhead, firstDigitIndex)
      }
    }
  }
}
