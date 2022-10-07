package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Monad
import fun.Applicative


/**
 * String-related functionality.
 */
object Strings:
  /**
   * Errors that may occur during string parsing.
   */
  trait Errors[M[_]]:
    /** Handles situation where input encounters invalid escape(d) character. */
    def invalidEscapeCharacter[T](): M[T]

    /**
     * Handles situation where unicode escape is not valid (not enough digits or
     * not a hexadecimal digit is present).
     */
    def invalidUnicodeEscape[T](): M[T]

    /**
     * Handles situation where string contains invalid character (that could not
     * occur regularly)
     */
    def invalidCharacter[T](): M[T]


    /** String is not properly terminated.  */
    def unterminatedString[T](): M[T]
  end Errors


  /** Special error - string is expected but start is different. */
  trait StartErrors[M[_]]:
    /** Illegal string start detected on the stream. */
    def illegalStringStart[T](): M[T]
  end StartErrors


  /**
   * Checks if the character is string boundary or not.
   */
  def isStringBoundary(char: Char): Boolean =
    char == '"'


  /**
   * Checks if the given character is an escape character.
   */
  def isEscapeCharacter(char: Char): Boolean =
    char == '\\'


  /**
   * Checks if the given character is an indicator of unicode escape.
   */
  def isUnicodeEscapeIndicator(char: Char): Boolean =
    char == 'u'


  /**
   * Checks if the given character is valid digit in the unicode escape.
   */
  def isUnicodeEscapeDigit(char: Char): Boolean =
    '0' <= char && char <= '9'
    || 'a' <= char && char <= 'f'
    || 'A' <= char && char <= 'F'


  /**
   * Checks if the given character is regular and valid string character.
   */
  def isRegularCharacter(char: Char): Boolean =
    char match
      case '\\' | '"' | '\r' | '\n' => false
      case x if x < 0x20 => false
      case _ => true
    end match
  end isRegularCharacter


  /**
   * Skips regular characters in the string content until string boundary,
   * escape or invalid character is encountered.
   * @param chars sequence to scan.
   * @param offset initial position to look at.
   * @return index of the first "irregular" string character.
   */
  def skipRegularCharacters(chars: CharSequence, offset: Int): Int =
    var ptr = offset
    while ptr < chars.length() && isRegularCharacter(chars.charAt(ptr)) do
      ptr += 1
    ptr
  end skipRegularCharacters


  /**
   * Starts reading the string (expecting the opening quote).
   * @param stream stream to read start of the string.
   * @return characters read and indicator if there is more data (i.e. `continueString`
   *   should be called).
   */
  def startString[M[_]: Monad: Errors: StartErrors](
        stream: CharacterStream[M]
      ): M[(CharSequence, Boolean)] =
    stream.peek(7) flatMap { lookAhead =>
      if lookAhead.length() <= 1 || !isStringBoundary(lookAhead.charAt(0)) then
        implicitly[StartErrors[M]].illegalStringStart()
      else
        processContent(lookAhead, stream, true)
    }
  end startString


  /**
   * Continues reading the string after initial portion (containing opening quote)
   * was read.
   * @param stream stream to read start of the string.
   * @return characters read and indicator if there is more data (i.e. `continueString`
   *   should be called).
   */
  def continueString[M[_]: Monad: Errors](
        stream: CharacterStream[M]
      ): M[(CharSequence, Boolean)] =
    stream.peek(6) flatMap { processContent(_, stream, false) }


  /** Creates new iterator-like pull string reader. */
  def newReader[M[_]: Monad: Errors: StartErrors](stream: CharacterStream[M]): StringReader[M] =
    new StringReader(stream)


  /** Reads the string fully. This may be memory-inefficient for huge numbers. */
  def readAll[M[_]: Monad: Errors: StartErrors](stream: CharacterStream[M]): M[String] =
    startString(stream) flatMap { (inputPortion, hasMore) =>
      if !hasMore then
        Monad.pure(inputPortion.toString())
      else
        val buffer = new StringBuilder()
        buffer.append(inputPortion)
        readAllImpl(stream, buffer)
    }


  /** Internal "accumulating" implementation of read-all. */
  private def readAllImpl[M[_]: Monad: Errors](
          stream: CharacterStream[M],
          buffer: StringBuilder,
      ): M[String] =
    continueString(stream) flatMap { (inputPortion, hasMore) =>
      buffer.append(inputPortion)
      if hasMore then
        readAllImpl(stream, buffer)
      else
        Monad.pure(buffer.toString())
    }


  /**
   * Processes a sequence of regular characters OR one escape character.
   * @param chars characters we are looking at to process.
   * @param stream stream with the data that produced characters. It will be advanced
   *   upon parsing.
   * @param isFirstChunk indicates if it is the first chunk (and the first character
   *   should be ignored).
   * @return contents of the string portion and indicator that there is more data.
   */
  private def processContent[M[_]: Monad: Errors](
          chars: CharSequence,
          stream: CharacterStream[M],
          isFirstChunk: Boolean
      ): M[(CharSequence, Boolean)] =
    val seqStart = if isFirstChunk then 1 else 0

    if chars.length() <= seqStart then
      if isFirstChunk then
        return stream.skip(1) flatMap { _ => implicitly[Errors[M]].unterminatedString() }
      else
        return implicitly[Errors[M]].unterminatedString()
    end if

    chars.charAt(seqStart) match
      /* Empty string. */
      case '"' => return stream.skip(seqStart + 1) map { _ => ("", false) }
      case '\\' => processEscape(chars, stream, isFirstChunk)
      case x if isRegularCharacter(x) =>
        val regCharsEnd = skipRegularCharacters(chars, seqStart)
        if regCharsEnd < chars.length() && isStringBoundary(chars.charAt(regCharsEnd)) then
          return stream.consume(regCharsEnd + 1) map { res => (res.subSequence(seqStart, regCharsEnd), false) }
        else if isFirstChunk then
          stream.consume(regCharsEnd) map { res => (res.subSequence(1, res.length()), true)}
        else
          stream.consume(regCharsEnd) map { res => (res, true) }
      case _ =>
        if isFirstChunk then
          return stream.skip(1) flatMap { _ => implicitly[Errors[M]].invalidCharacter() }
        else
          implicitly[Errors[M]].invalidCharacter()
    end match
  end processContent


  /**
   * Processes an escape character.
   * @param chars characters we are looking at to process.
   * @param stream stream with the data that produced characters. It will be advanced
   *   upon parsing.
   * @param isFirstChunk indicates if it is the first chunk (and the first character
   *   should be ignored).
   * @return contents of the string portion and indicator that there is more data.
   */
  private def processEscape[M[_]: Monad: Errors](
          chars: CharSequence,
          stream: CharacterStream[M],
          isFirstChunk: Boolean
      ): M[(CharSequence, Boolean)] =
    val escapeStart = if isFirstChunk then 1 else 0
    val escapeCodeIndex = escapeStart + 1

    if chars.length() <= escapeCodeIndex then
      return stream.skip(escapeStart) flatMap { _ => implicitly[Errors[M]].unterminatedString() }

    val (ret, escapeLength) =
      chars.charAt(escapeCodeIndex) match
        case '"' => ("\"", 1)
        case '\\' => ("\\", 1)
        case '/' => ("/", 1)
        case 'b' => ("\b", 1)
        case 'f' => ("\f", 1)
        case 'n' => ("\n", 1)
        case 'r' => ("\r", 1)
        case 't' => ("\t", 1)
        case 'u' =>
          if chars.length() <= escapeCodeIndex + 4 then
            if isFirstChunk then
              return stream.skip(1) flatMap { _ => implicitly[Errors[M]].invalidUnicodeEscape() }
            else
              return implicitly[Errors[M]].invalidUnicodeEscape()

          var charCode = 0
          var charCharPtr = escapeCodeIndex + 1

          val unicodeEndIndex = charCharPtr + 4

          while charCharPtr < unicodeEndIndex do
            chars.charAt(charCharPtr) match
              case x if '0' <= x && x <= '9' => charCode = (charCode << 4) | (x - '0')
              case x if 'a' <= x && x <= 'f' => charCode = (charCode << 4) | (x - 'a' + 10)
              case x if 'A' <= x && x <= 'F' => charCode = (charCode << 4) | (x - 'A' + 10)
              case other =>
                if isFirstChunk then
                  return stream.skip(1) flatMap { _ => implicitly[Errors[M]].invalidUnicodeEscape() }
                else
                  return implicitly[Errors[M]].invalidUnicodeEscape()
            end match
            charCharPtr += 1
          end while

          (String.valueOf(charCode.toChar), 5)
        case other =>
          if isFirstChunk then
            return stream.skip(1) flatMap { _ => implicitly[Errors[M]].invalidEscapeCharacter() }
          else
            return implicitly[Errors[M]].invalidEscapeCharacter()
      end match

    var ptr = escapeCodeIndex + escapeLength

    /* Try to look-up termination early. */
    if ptr < chars.length() && isStringBoundary(chars.charAt(ptr)) then
      return stream.skip(ptr + 1) map { _ => (ret, false) }
    else
      return stream.skip(ptr) map { _ => (ret, true) }
  end processEscape

end Strings

