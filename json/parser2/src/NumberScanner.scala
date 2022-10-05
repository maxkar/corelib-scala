package io.github.maxkar
package json.parser

/**
 * Stateful "scanner" over the stream that contains number where stream is controlled
 * by the scanner's client.
 *
 * The following pseudocode illustrates the client using the scanner:
 * ```
 * val scanner = Numbers.scanner()
 *
 * var nextChars = stream.read()
 * while scanner.acceptsInput() do
 *   val consumed = scanner.accept(nextChars)
 *   // return unconsumed characters to the stream
 *   // unconsumed are characters starting from the `consumed` position
 * scanner.end() match
 *   case NumberScanner.Success => ...
 *   ...
 * ```
 */
final class NumberScanner:
  import NumberScanner._

  /** Outcome of the parsing. */
  private var result: Result = null

  /** Current parsing state. */
  private var state = State.Start

  /**
   * Checks if additional input may update the state of this parser. If `true`,
   * more calls to `accept` may be made. If `false`, then no more input would
   * make any state changes. The `end` method should be called to get scanner's result.
   */
  def acceptsInput(): Boolean = result == null

  /**
   * Finishes parsing (if not already finished due to input format or another call to
   * the `end` and returns scanner's opinion about the input seen so far).
   */
  def end(): Result =
    if result != null then return result

    result =
      state match
        case State.AfterLeading0 | State.InsideIntegerDigits | State.InsideDecimalDigits | State.InsideExponentDigits => Result.Success
        case State.Start | State.AfterNumberSign => Result.MissingIntegerDigits
        case State.AfterDecimalIndicator => Result.MissingDecimalDigits
        case State.AfterExponentIndicator | State.AfterExponentSign => Result.MissingExponentDigits
      end match
    result
  end end


  /**
   * Updates the state from the next input characters.
   *
   * The parser returns number of characters used to update its internal state.
   * If the returned value is less than `nextCharacters.length()` then
   * the parser is in its final state and `this.accepsInput()` will always return `false`.
   *
   * @return number of characters used to update the state.
   */
  def accept(nextChars: CharSequence): Int =
    if result != null then return 0

    state match
      case State.Start => startNumber(nextChars, 0)
      case State.AfterNumberSign => startIntegerPart(nextChars, 0)
      case State.AfterLeading0 => afterLeading0(nextChars, 0)
      case State.InsideIntegerDigits => insideIntegerDigits(nextChars, 0)
      case State.AfterDecimalIndicator => startDecimalDigits(nextChars, 0)
      case State.InsideDecimalDigits => insideDecimalDigits(nextChars, 0)
      case State.AfterExponentIndicator => startExponentSign(nextChars, 0)
      case State.AfterExponentSign => startExponentDigits(nextChars, 0)
      case State.InsideExponentDigits => insideExpDigits(nextChars, 0)
    end match
  end accept


  /** Starts the number. */
  private def startNumber(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if Numbers.isNumberSign(chars.charAt(start)) then
      state = State.AfterNumberSign
      startIntegerPart(chars, start + 1)
    else
      startIntegerPart(chars, start)
    end if
  end startNumber


  /** Starts the integer part. */
  private def startIntegerPart(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if '0' == chars.charAt(start) then
      state = State.AfterLeading0
      afterLeading0(chars, start + 1)
    else if Numbers.isDigit(chars.charAt(start)) then
      state = State.InsideIntegerDigits
      insideIntegerDigits(chars, start)
    else
      result = Result.MissingIntegerDigits
      start
    end if
  end startIntegerPart


  /** Processes situation after leading 0. */
  private def afterLeading0(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if Numbers.isDigit(chars.charAt(start)) then
      result = Result.DigitsAfterLeading0InIntegerPart
      start
    else
      startDecimal(chars, start)
    end if
  end afterLeading0


  /** Processes situation inside integer part. */
  private def insideIntegerDigits(chars: CharSequence, start: Int): Int =
    val nonDigit = Numbers.skipDigits(chars, start)
    if nonDigit < chars.length() then
      startDecimal(chars, nonDigit)
    else
      nonDigit
    end if
  end insideIntegerDigits


  /** Starts optional decimal part. */
  private def startDecimal(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if Numbers.isDecimalSeparator(chars.charAt(start)) then
      state = State.AfterDecimalIndicator
      startDecimalDigits(chars, start + 1)
    else
      startExponent(chars, start)
    end if
  end startDecimal


  /** Starts processing of the decimal digits (requires at least one digit). */
  private def startDecimalDigits(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if Numbers.isDigit(chars.charAt(start)) then
      state = State.InsideDecimalDigits
      insideDecimalDigits(chars, start + 1)
    else
      result = Result.MissingDecimalDigits
      start
    end if
  end startDecimalDigits


  /** Processes decimal digits (inside the sequence). */
  private def insideDecimalDigits(chars: CharSequence, start: Int): Int =
    val nonDigit = Numbers.skipDigits(chars, start)
    if nonDigit < chars.length() then
      startExponent(chars, nonDigit)
    else
      nonDigit
    end if
  end insideDecimalDigits


  /** Starts an optional exponent. */
  private def startExponent(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if Numbers.isExponentIndicator(chars.charAt(start)) then
      state = State.AfterExponentIndicator
      startExponentSign(chars, start + 1)
    else
      result = Result.Success
      start
    end if
  end startExponent


  /** Processes an exponent sign. */
  private def startExponentSign(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if Numbers.isExponentSign(chars.charAt(start)) then
      state = State.AfterExponentSign
      startExponentDigits(chars, start + 1)
    else
      startExponentDigits(chars, start)
    end if
  end startExponentSign


  /** Prosses the initial exponent digit(s). */
  private def startExponentDigits(chars: CharSequence, start: Int): Int =
    if start >= chars.length() then
      start
    else if Numbers.isDigit(chars.charAt(start)) then
      state = State.InsideExponentDigits
      insideExpDigits(chars, start + 1)
    else
      result = Result.MissingExponentDigits
      start
    end if
  end startExponentDigits


  /** Processes exponent digits. */
  private def insideExpDigits(chars: CharSequence, start: Int): Int =
    val ret = Numbers.skipDigits(chars, start)
    if ret < chars.length() then
      result = Result.Success
    ret
  end insideExpDigits


end NumberScanner

object NumberScanner:
  /** State of the scanner - where we are in the sequence. */
  private enum State:
    /** The very initial state - before the integer part and sign. */
    case Start
    /** Number sign was read. */
    case AfterNumberSign
    /** Initial (leading) 0 was read (no other digits). */
    case AfterLeading0
    /** At least one integer digit (which was not 0) was read. */
    case InsideIntegerDigits
    /** Decimal indicator was read. */
    case AfterDecimalIndicator
    /** At least one decimal digit was read. */
    case InsideDecimalDigits
    /** An exponent indicator was read. */
    case AfterExponentIndicator
    /** Exponent sign was read. */
    case AfterExponentSign
    /** At least one exponent digit was read. */
    case InsideExponentDigits
  end State


  /** Result of parsing/consuming digits. */
  abstract sealed class Result

  /** Result was a failure. Concrete objects indicate what was the failure. */
  abstract sealed class Failure extends Result

  object Result:
    /** Parsing was successfull. */
    case object Success extends Result


    /** Digits encountered after leading 0 in the integer part. */
    case object DigitsAfterLeading0InIntegerPart extends Failure

    /** Integer part (digits) is missing in the input. */
    case object MissingIntegerDigits extends Failure

    /**
     * Decimal digits should be present (due to decimal part indicator)
     * but is missing from the input.
     */
    case object MissingDecimalDigits extends Failure

    /**
     * Exponent digits should be present (due to decimal part indicator)
     * but is missing from the input.
     */
    case object MissingExponentDigits extends Failure
  end Result

end NumberScanner
