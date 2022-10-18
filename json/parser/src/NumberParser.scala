package io.github.maxkar
package json.parser

import fun.typeclass.Monad

/**
 * Parser for numeric value.
 * @tparam M operation type.
 * @tparam J JSON value representation type.
 *
 * @param parserInput input stream/input capabilities.
 * @param jsonFactory json element factory.
 */
private class NumberParser[M[_]: Monad, J](
      parserInput: input.ParserInput[M],
      jsonFactory: factory.NumberFactory[M, J]):

  import NumberParser._

  /** State of the value scanning. */
  private type ScanState = (State, jsonFactory.State)


  /** Parses the number. */
  def parse: M[J] =
    for
      peerState <- jsonFactory.begin
      (myState, finalPeerState) <- parserInput.statefulScan((State.Start, peerState), updateState)
      res <-
        myState match
          case State.Start | State.AfterSign => jsonFactory.missingIntDigits(finalPeerState)
          case State.AfterLeading0 | State.InsideIntPart => jsonFactory.end(finalPeerState)
          case State.AfterDecimalDot => jsonFactory.missingFractionalDigits(finalPeerState)
          case State.InsideDecimalDigits => jsonFactory.end(finalPeerState)
          case State.AfterExpIndicator | State.AfterExpSign => jsonFactory.missingExponentDigits(finalPeerState)
          case State.InsideExpDigits => jsonFactory.end(finalPeerState)
          case State.FactoryAbort => jsonFactory.end(finalPeerState)
          case State.NumberWithLeading0 => jsonFactory.digitsAfterLeadingZero(finalPeerState)
        end match
    yield res


  /** Scans the input and updates the state based on the provided characters. */
  private def updateState(buffer: CharSequence, state: ScanState): (input.ConsumerStatus, ScanState) =
    val (myState, peerState) = state
    val scanner = new Scanner(myState, buffer)
    val newOffset = scanner.scan()
    if newOffset == 0 then
      (input.ConsumerStatus.Finished(0), (scanner.state, peerState))
    else
      val (newPeerState, shouldContinue) = jsonFactory.update(peerState, buffer.subSequence(0, newOffset))
      val myNewState = if shouldContinue then scanner.state else State.FactoryAbort
      val cmd =
        if newOffset < buffer.length then
          input.ConsumerStatus.Finished(newOffset)
        else
          input.ConsumerStatus.NeedMoreInput
      (cmd, (myNewState, newPeerState))
    end if
  end updateState



end NumberParser


object NumberParser:

  /** Parsing state. */
  private enum State:
    /** Start of the number - no input so far. */
    case Start
    /** Sign was consumed, skipping over integer part. */
    case AfterSign
    /** After leading 0. */
    case AfterLeading0
    /** Number with leading zero occured. */
    case NumberWithLeading0
    /** Inside the integer part. */
    case InsideIntPart
    /** After decimal dot. */
    case AfterDecimalDot
    /** Inside decimal digits. */
    case InsideDecimalDigits
    /** After exponent indicator. */
    case AfterExpIndicator
    /** After exponent sign. */
    case AfterExpSign
    /** Inside some exponent digits. */
    case InsideExpDigits
    /** Abort was requested by the factory. */
    case FactoryAbort
  end State


  /**
   * Scanner over the `buf` sequence.
   * @param state current state (may change over scanning).
   * @param buf buffer to parse.
   */
  private class Scanner(var state: State, buf: CharSequence):

    /** Continues parsing based on the current state. */
    def scan(): Int =
      state match
        case State.Start =>
          if buf.charAt(0) == '-' then
            state = State.AfterSign
            handleAfterSign(1)
          else
            handleAfterSign(0)
        case State.AfterSign => handleAfterSign(0)
        case State.AfterLeading0 => handleAfter0(0)
        case State.NumberWithLeading0 => 0
        case State.InsideIntPart => handleIntPart(0)
        case State.AfterDecimalDot => handleDecimal(0)
        case State.InsideDecimalDigits => handleDecimal(0)
        case State.AfterExpIndicator => skipAfterExpIndicator(0)
        case State.AfterExpSign => skipDigits(0, State.InsideExpDigits)
        case State.InsideExpDigits => skipDigits(0, State.InsideExpDigits)
        case State.FactoryAbort => 0
      end match
    end scan


    /** Handles the situation after the number sign. */
    private def handleAfterSign(offset: Int): Int =
      if offset == buf.length then
        offset
      else if buf.charAt(offset) == '0' then
        state = State.AfterLeading0
        handleAfter0(offset + 1)
      else
        handleIntPart(offset)
    end handleAfterSign


    /** Handles the situation after the leading 0. */
    private def handleAfter0(offset: Int): Int =
      if offset == buf.length then
        offset
      else if Character.isDigit(buf.charAt(offset)) then
        state = State.NumberWithLeading0
        offset
      else
        handleFracAndExp(offset)
    end handleAfter0


    /** Handles the situation inside the integer part. */
    private def handleIntPart(offset: Int): Int =
      val newOffset = skipDigits(offset, State.InsideIntPart)

      if newOffset == buf.length then
        newOffset
      else if state == State.InsideIntPart then
        handleFracAndExp(newOffset)
      else
        newOffset
    end handleIntPart


    /** Dispatches the processing after the integer part. Expects non-empty input. */
    private def handleFracAndExp(offset: Int): Int =
      val nextChar = buf.charAt(offset)
      if nextChar == '.' then
        state = State.AfterDecimalDot
        handleDecimal(offset + 1)
      else if isExponentIndicator(nextChar) then
        state = State.AfterExpIndicator
        skipAfterExpIndicator(offset + 1)
      else
        offset
    end handleFracAndExp


    /** Handles decimal digits. */
    private def handleDecimal(offset: Int): Int =
      val newOffset = skipDigits(offset, State.InsideDecimalDigits)

      if newOffset < buf.length && state == State.InsideDecimalDigits && isExponentIndicator(buf.charAt(newOffset)) then
        state = State.AfterExpIndicator
        skipAfterExpIndicator(newOffset + 1)
      else
        newOffset

    end handleDecimal


    /** Handles the situation after the exponent indicator. */
    private def skipAfterExpIndicator(offset: Int): Int =
      if offset == buf.length then
        offset
      else if isSign(buf.charAt(offset)) then
        state = State.AfterExpSign
        skipDigits(offset + 1, State.InsideExpDigits)
      else
        skipDigits(offset, State.InsideExpDigits)
    end skipAfterExpIndicator


    /**
     * Skips over the digit section of the input. Applies the `newState` if
     * any digits were skipped.
     */
    private def skipDigits(offset: Int, newState: State): Int =
      var ptr = offset
      while ptr < buf.length && Character.isDigit(buf.charAt(ptr)) do
        ptr += 1
      if ptr > offset then
        state = newState
      ptr
    end skipDigits

    /** Checks if the given character is sign char. */
    private def isSign(c: Char): Boolean =
      c == '-' || c == '+'

    /** Checks if the character is an exponent indicator. */
    private def isExponentIndicator(c: Char): Boolean =
      c == 'e' || c == 'E'

  end Scanner

end NumberParser
