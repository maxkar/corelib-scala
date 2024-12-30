package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad

import text.v2.input.Reader
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn

/**
 * Reader for JSON Strings.
 * @tparam T type of the underlying stream.
 */
final class StringReader[T](private val stream: T) {
  /** State of the reading (i.e. what we would read next). */
  private var state: StringReader.State = StringReader.State.BeforeOpening
}


/** Reader for JSON strings. */
object StringReader {
  /**
   * Error handlers for JSON strings.
   * @tparam M execution (monad).
   */
  trait Errors[M[_], S] {
    /**
     * Invoked when string start was expected but something different was present
     * on the stream.
     */
    def illegalStringStart[T](stream: S): M[T]

    /**
     * Invoked when string contains some escaped character (like '\n') but the escape
     * code is not the valid one.
     */
    def invalidEscapeCharacter[T](stream: S): M[T]

    /**
     * Invoked when string contains some unicode escape (like '\u0058') but the encoding
     * is not valid (i.e. digits are not valid).
     */
    def invalidUnicodeEscape[T](stream: S): M[T]

    /**
     * Invoked when string contains a character that could not be present (i.e. the one
     * that should always be present as escape character).
     */
    def invalidCharacter[T](stream: S): M[T]


    /**
     * Invoked when string is not properly terminated. Practially this means that the
     * end of stream was reached but no stream terminator character was found.
     */
    def unterminatedString[T](stream: S): M[T]
  }

  type ErrorsIn[M[_]] = [S] =>> Errors[M, S]


  /** State of the reading. */
  enum State {
    /** We are before the opening quote. */
    case BeforeOpening
    /** We are inside string. */
    case InString
    /** We are after the closing character. */
    case Eof
  }


  /** Creates a new string reader. */
  inline def apply[T](base: T): StringReader[T] = new StringReader(base)


  given reader[M[_]: Monad, T: LooksAheadIn[M]](using errs: Errors[M, T]): Reader[M, StringReader[T]] with {
    override def read(
          source: StringReader[T],
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int)
        : M[Int] =
      source.state match {
        case State.BeforeOpening =>
          source.stream.peek(0) flatMap { chr =>
            if chr == '"' then
              source.stream.skip(1) <+> {
                source.state = State.InString
                readStringBody(source, target, targetStart, targetEnd)
              }
            else
              errs.illegalStringStart(source.stream)
          }
        case State.InString =>
          readStringBody(source, target, targetStart, targetEnd)
        case State.Eof => Monad.pure(-1)
      }


    /** Reads "body" of the string (part between the string quotes). */
    private def readStringBody(
          source: StringReader[T],
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int
        ): M[Int] = {
      val stream = source.stream

      /** Fills the buffer with data. */
      def fill(ptr: Int): M[Int] = {
        if ptr == targetEnd then
          return Monad.pure(ptr - targetStart)

        stream.readWhile(target, ptr, targetEnd, isRegularCharacter) <||| { rdc =>
          if rdc >= 0 then
            readSpecial(ptr + rdc)
          else if ptr == targetStart then
            errs.unterminatedString(stream)
          else
            Monad.pure(ptr - targetStart)
        }
      }

      /* Reads one special character. */
      def readSpecial(ptr: Int): M[Int] = {
        if ptr == targetEnd then
          return Monad.pure(ptr - targetStart)
        stream.peek(0) <||| {
          case '"' =>
            source.state = State.Eof
            stream.skip(1) <+> Monad.pure(ptr - targetStart)
          case '\\' =>
            readEscape(stream) <||| { nc =>
              target(ptr) = nc
              fill(ptr + 1)
            }
          case x if x < 0 => errs.unterminatedString(stream)
          case other => errs.invalidCharacter(stream)
        }
      }

      fill(targetStart)
    }


    /** Reads an escaped character. */
    private def readEscape(stream: T): M[Char] =
      stream.peek(1) <||| {
        case '"' => stream.skip(2) <| { _ => '"' }
        case '\\' => stream.skip(2) <| { _ => '\\' }
        case '/' => stream.skip(2) <| { _ => '/' }
        case 'b' => stream.skip(2) <| { _ => '\b' }
        case 'f' => stream.skip(2) <| { _ => '\f' }
        case 'n' => stream.skip(2) <| { _ => '\n' }
        case 'r' => stream.skip(2) <| { _ => '\r' }
        case 't' => stream.skip(2) <| { _ => '\t' }
        case 'u' => readUnicodeEscape(stream)
        case other => errs.invalidEscapeCharacter(stream)
      }


    /** Reads unicode escape. */
    private def readUnicodeEscape(stream: T): M[Char] = {
      for {
        _ <- stream.fill(6)
        c1 <- peekHexDigit(stream, 2)
        c2 <- peekHexDigit(stream, 3)
        c3 <- peekHexDigit(stream, 4)
        c4 <- peekHexDigit(stream, 5)
        _ <- stream.skip(6)
      } yield
        ((c1 << 12) | (c2 << 8) | (c3 << 4) | c4).toChar
    }


    /** Loads hex digit from the stream. */
    private def peekHexDigit(stream: T, offset: Int): M[Int] =
      stream.peek(offset) <||| {
        case d if '0' <= d && d <= '9' => Monad.pure(d - '0')
        case d if 'a' <= d && d <= 'f' => Monad.pure(10 + d - 'a')
        case d if 'A' <= d && d <= 'F' => Monad.pure(10 + d - 'A')
        case _ => errs.invalidUnicodeEscape(stream)
      }
  }


  /**
   * Checks if the given character is regular and valid string character.
   */
  def isRegularCharacter(char: Char): Boolean =
    char match {
      case '\\' | '"' | '\r' | '\n' => false
      case x if x < 0x20 => false
      case _ => true
    }
}
