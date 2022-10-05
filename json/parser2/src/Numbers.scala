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
 *
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
  /** Synonym for the NumberScanner type for convenient access. */
  type Scanner = NumberScanner
  /** Synonym for the NumberScanner companion for convenient access. */
  val Scanner = NumberScanner

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
        implicitly[Errors[M]].missingIntegerDigits()
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
  def readOptionalExponentIndicator[M[_]: Monad](
        stream: CharacterStream[M]
      ) : M[Option[Char]] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() > 0 && isExponentIndicator(lookAhead.charAt(0)) then
        stream.consume(1) map { rd => Some(rd.charAt(0)) }
      else
        Monad.pure(None)
    }
  end readOptionalExponentIndicator


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
        stream.consume(1) map { rd => rd.charAt(0) }
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
        readAllImpl(stream, buffer, state)
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
      stream.peek(1) flatMap { lookAhead =>
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
      stream.peek(1) flatMap { lookAhead =>
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
