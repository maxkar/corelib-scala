package io.github.maxkar
package json.parser

import fun.typeclass.Monad

/**
 * Parser for string value.
 *
 * @tparam M operation type.
 * @tparam J JSON value representation type.
 *
 * @param parserInput input stream/input capabilities.
 * @param jsonFactory json element factory.
 */
class StringParser[M[_]: Monad, J](
      parserInput: input.ParserInput[M],
      jsonFactory: factory.StringFactory[M, J]):

  import StringParser._

  /** State of the value scanning. */
  private type ScanState = (State, jsonFactory.State)


  /** Starts parsing. */
  def parse: M[J] =
    for
      peerState <- jsonFactory.begin
      _ <- parserInput.skipChar()
      (myState, newPeerState) <- parserInput.statefulScan((State.Normal, peerState), updateState)
      res <-
        myState match
          case State.End | State.FactoryAbort => jsonFactory.end(peerState)
          case State.Normal => jsonFactory.unterminatedString(newPeerState)
          case State.EscapeStart => jsonFactory.badEscape(newPeerState)
          case State.Unicode0 => jsonFactory.badUnicodeEscape(newPeerState)
          case State.Unicode1(_, c0) => jsonFactory.badUnicodeEscape(newPeerState, c0)
          case State.Unicode2(_, c0, c1) => jsonFactory.badUnicodeEscape(newPeerState, c0, c1)
          case State.Unicode3(_, c0, c1, c2) => jsonFactory.badUnicodeEscape(newPeerState, c0, c1, c2)
          case State.BadInput => jsonFactory.badCharacter(peerState)
        end match
    yield
      res
  end parse


  /** Scans the input and updates the state based on the provided characters. */
  private def updateState(buffer: CharSequence, state: ScanState): (input.ConsumerStatus, ScanState) =
    var ptr = 0
    var (myState, peerState) = state

    while ptr < buffer.length do
      myState match
        case State.Normal =>
          val start = ptr
          while ptr < buffer.length && isRegular(buffer.charAt(ptr)) do
            ptr += 1

          if ptr > start then
            val (nps, shouldContinue) = jsonFactory.update(peerState, buffer.subSequence(start, ptr))
            if (!shouldContinue)
              return (input.ConsumerStatus.Finished(ptr), (State.FactoryAbort, nps))
            else peerState = nps

          if ptr < buffer.length then
            buffer.charAt(ptr) match
              case '\\' =>
                myState = State.EscapeStart
                ptr += 1
              case '"' =>
                return (input.ConsumerStatus.Finished(ptr + 1), (State.End, peerState))
              case _ =>
                return (input.ConsumerStatus.Finished(ptr), (State.BadInput, peerState))
        case State.EscapeStart =>
          val escaped =
            buffer.charAt(ptr) match
              case '"' => '"'
              case '\\' => '\\'
              case '/' => '/'
              case 'b' => '\b'
              case 'f' => '\f'
              case 'n' => '\n'
              case 'r' => '\r'
              case 't' => '\t'
              case 'u' =>
                ptr += 1
                myState = State.Unicode0
                '\u0000'
              case _ =>
                return (input.ConsumerStatus.Finished(ptr), (State.EscapeStart, peerState))
            end match

          if escaped > 0 then
            val (nps, shouldContinue) = jsonFactory.update(peerState, escaped)
            if (!shouldContinue)
              return (input.ConsumerStatus.Finished(ptr), (State.FactoryAbort, nps))
            else
              ptr += 1
              myState = State.Normal
              peerState = nps

        case State.Unicode0 =>
          val c = buffer.charAt(ptr)
          val digit = toDigit(c)
          if digit >= 0 then
            myState = State.Unicode1(digit, c)
            ptr += 1
          else
            return (input.ConsumerStatus.Finished(ptr), (myState, peerState))

        case State.Unicode1(num, c0) =>
          val c = buffer.charAt(ptr)
          val digit = toDigit(c)
          if digit >= 0 then
            myState = State.Unicode2((num << 4) | digit, c0, c)
            ptr += 1
          else
            return (input.ConsumerStatus.Finished(ptr), (myState, peerState))

        case State.Unicode2(num, c0, c1) =>
          val c = buffer.charAt(ptr)
          val digit = toDigit(c)
          if digit >= 0 then
            myState = State.Unicode3((num << 4) | digit, c0, c1, c)
            ptr += 1
          else
            return (input.ConsumerStatus.Finished(ptr), (myState, peerState))

        case State.Unicode3(num, c0, c1, c2) =>
          val c = buffer.charAt(ptr)
          val digit = toDigit(c)
          if digit >= 0 then
            ptr += 1
            val (nps, shouldContinue) = jsonFactory.update(peerState, ((num << 4) | digit).toChar)
            if (!shouldContinue)
              return (input.ConsumerStatus.Finished(ptr), (State.FactoryAbort, nps))
            myState = State.Normal
            peerState = nps
          else
            return (input.ConsumerStatus.Finished(ptr), (myState, peerState))

        case State.End | State.BadInput | State.FactoryAbort =>
          return (input.ConsumerStatus.Finished(ptr), (myState, peerState))
      end match
    end while

    (input.ConsumerStatus.NeedMoreInput, (myState, peerState))
  end updateState


  /** Checks if the character is a regular string character. */
  private def isRegular(c: Char): Boolean =
    c match
      case '\\' | '"' | '\r' | '\n' => false
      case x if x < 0x20 => false
      case _ => true
  end isRegular


  /** Converts the character into a digit. */
  private def toDigit(c: Char): Int =
    if '0' <= c && c <= '9' then c - '0'
    else if 'a' <= c && c <= 'f' then c - 'a' + 10
    else if 'A' <= c && c <= 'F' then c - 'A' + 10
    else -1


end StringParser


object StringParser:
  /** Parsing state. */
  private enum State:
    /** Inside the normal string (i.e. regular characters). */
    case Normal
    /** After the '\' character (expecting unicode input). */
    case EscapeStart
    /** Unicode escape is expected. */
    case Unicode0
    /**
     * One character of unicode was read.
     * @param v "value so far"
     * @param c0 initial (input) character.
     */
    case Unicode1(v: Int, c0: Char)
    /**
     * Two characters of unicode were read.
     * @param v "value so far"
     * @param c0 initial (input) character.
     * @param c1 second (input) character.
     */
    case Unicode2(v: Int, c0: Char, c1: Char)
    /**
     * Three characters of unicode were read.
     * @param v "value so far"
     * @param c0 initial (input) character.
     * @param c1 second (input) character.
     * @param c2 third (input) character.
     */
    case Unicode3(v: Int, c0: Char, c1: Char, c2: Char)
    /** String terminator was read. */
    case End
    /** Invalid (illegal) character occured in the input.  */
    case BadInput
    /** Abort was requested by value factory. */
    case FactoryAbort
  end State

end StringParser
