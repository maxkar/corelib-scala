package io.github.maxkar
package json.parser.v2

import fun.typeclass.Monad

import text.v2.input.Reader
import text.v2.input.LookAhead
import text.v2.input.LooksAheadIn

/**
 * Reader for JSON Strings.
 * @tparam S type of the underlying stream.
 */
final class NumberReader[S](private val stream: S) {
  /** State of the reading (i.e. what we would read next). */
  private var state: NumberReader.State = NumberReader.State.BeforeNumber
}


/** Reader for JSON numbers. */
object NumberReader {
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
  type ErrorsIn[M[_]] = [S] =>> Errors[M, S]


  object Errors {
    def raise[M[_], S](raiseFn: [T] => (S, String) => M[T]): Errors[M, S] =
      new Errors[M, S] {
        override def missingIntegerDigits[T](stream: S): M[T] =
          raiseFn(stream, "Missing integer digits")
        override def leadingIntegerZero[T](stream: S): M[T] =
          raiseFn(stream, "Leading zero is not allowed")
        override def missingDecimalDigits[T](stream: S): M[T] =
          raiseFn(stream, "Missing decimal digits")
        override def missingExponentDigits[T](stream: S): M[T] =
          raiseFn(stream, "Missing exponent digits")
      }
  }


  /** State of the reading. */
  enum State {
    /** We are before the integer part. */
    case BeforeNumber
    /** We are after sign but before the integer part. */
    case AfterSign
    /** We are inside integer part. */
    case InInteger
    /** We are after the decimal separator. */
    case AfterDecimalSeparator
    /** We are inside the decimal part. */
    case InDecimal
    /** We are after the exponent separator. */
    case AfterExponentSeparator
    /** We are after the exponent sign. */
    case AfterExponentSign
    /** We are inside the exponent part. */
    case InExponent
    /** We are after the number. */
    case Eof
  }


  /**
   * Context for reading stream of type S.
   * @param source source being read.
   * @param target target to fill.
   * @param targetStart startingPosition (as supplied by the user).
   * @param targetEnd ending position (as supplied by the user).
   */
  private class Context[S](
        val source: NumberReader[S],
        val target: Array[Char],
        val targetStart: Int,
        val targetEnd: Int,
      ) {
    /** Stream we are reading. */
    val stream: S = source.stream
    /** Current offset in the target. */
    var ptr = targetStart

    /** Checks if end of output was reached. */
    def endOfOutput(): Boolean = ptr == targetEnd

    /**
     * Number of characters read/copied (positive value) or -1 if
     * no characters were written (and thus end-of-input was reached).
     */
    def charactersWritten(): Int =  {
      val res = ptr - targetStart
      if res == 0 then -1 else res
    }

    /** Switches to a target state. */
    def goTo(state: State): Unit = source.state = state
  }


  /** Creates a new string reader. */
  inline def apply[S](base: S): NumberReader[S] = new NumberReader(base)


  given reader[M[_]: Monad, S: LooksAheadIn[M]](using errs: Errors[M, S]): Reader[M, NumberReader[S]] with {

    override def read(
          source: NumberReader[S],
          target: Array[Char],
          targetStart: Int,
          targetEnd: Int)
        : M[Int] =
      read(new Context(source, target, targetStart, targetEnd))


    /** Reads number in the context. */
    private def read(ctx: Context[S]): M[Int] =
      ctx.source.state match {
        case State.BeforeNumber => readNumberSign(ctx)
        case State.AfterSign => readNumberStart(ctx)
        case State.InInteger => readDigits(ctx, readDecimalPart)
        case State.AfterDecimalSeparator => startDecimalDigits(ctx)
        case State.InDecimal => readDigits(ctx, readExponentPart)
        case State.AfterExponentSeparator => readExponentSign(ctx)
        case State.AfterExponentSign => startExponentDigits(ctx)
        case State.InExponent => readDigits(ctx, endNumber)
        case State.Eof => Monad.pure(-1)
      }



    /** Reads number sign. */
    private def readNumberSign(ctx: Context[S]): M[Int] =
      ctx.stream.peek(0) <||| {
        case '-' => readOneAndContinue(ctx, State.AfterSign)
        case other => goToAndContinue(ctx, State.AfterSign)
      }


    /** Reads number start. */
    private def readNumberStart(ctx: Context[S]): M[Int] =
      ctx.stream.peek(1) <||| { la2 =>
        ctx.stream.peek(0) <||| { la1 =>
          if !isDigit(la1) then
            errs.missingIntegerDigits(ctx.stream)
          else if la1 == '0' && isDigit(la2) then
            errs.leadingIntegerZero(ctx.stream)
          else {
            ctx.goTo(State.InInteger)
            readDigits(ctx, readDecimalPart)
          }
        }
      }


    /** Reads fractional part. */
    private def readDecimalPart(ctx: Context[S]): M[Int] =
      ctx.stream.peek(0) <||| {
        case '.' => readOneAndContinue(ctx, State.AfterDecimalSeparator)
        case other => readExponentPart(ctx)
      }


    /** Reads decimal digits. */
    private def startDecimalDigits(ctx: Context[S]): M[Int] =
      startDigits(ctx, State.InDecimal, errs.missingDecimalDigits, readExponentPart)


    /** Reads exponent part. */
    private def readExponentPart(ctx: Context[S]): M[Int] =
      ctx.stream.peek(0) <||| {
        case 'e' | 'E' => readOneAndContinue(ctx, State.AfterExponentSeparator)
        case _ =>
          ctx.goTo(State.Eof)
          Monad.pure(ctx.charactersWritten())
      }


    /** Reads exponent sign. */
    private def readExponentSign(ctx: Context[S]): M[Int] =
      ctx.stream.peek(0) <||| { maybeSign =>
        if isExponentSign(maybeSign) then
          readOneAndContinue(ctx, State.AfterExponentSign)
        else
          goToAndContinue(ctx, State.AfterExponentSign)
      }


    /** Reads exponent digits. */
    private def startExponentDigits(ctx: Context[S]): M[Int] =
      startDigits(ctx, State.InExponent, errs.missingExponentDigits, endNumber)


    /** Ends processing. */
    private def endNumber(ctx: Context[S]): M[Int] = {
      ctx.goTo(State.Eof)
      Monad.pure(ctx.charactersWritten())
    }


    /** Goes to a new state (without reading) and continues reading. */
    private def goToAndContinue(ctx: Context[S], state: State): M[Int] = {
      ctx.goTo(state)
      read(ctx)
    }


    /** Reads one charactes, goes to a new state and continues reading. */
    private def readOneAndContinue(ctx: Context[S], state: State): M[Int] = {
      val start = ctx.ptr
      ctx.ptr += 1
      ctx.goTo(state)
      ctx.stream.read(ctx.target, start, ctx.ptr) <+> {
        if ctx.endOfOutput() then
          Monad.pure(ctx.charactersWritten())
        else
          read(ctx)
      }
    }


    /**
     * Reads starting digits of some part of the number (decimal or exponent).
     * Invokes the `next` function if there are no more digits to read but the
     * output buffer is not full. Invokes the `onError` function if there are no
     * digits at all. Switches stream to the `state` for reading.
     */
    private def startDigits(
          ctx: Context[S],
          state: State,
          onError: S => M[Int],
          next: Context[S] => M[Int]
        ): M[Int] =
      ctx.stream.peek(0) <||| { maybeDigit =>
        if !isDigit(maybeDigit) then
          onError(ctx.stream)
        else {
          ctx.goTo(state)
          readDigits(ctx, next)
        }
      }

    /**
     * Reads (non-starting) digits from the context. Invokes the `next` function
     * if there are no more digits to read but the output buffer is not full.
     */
    private def readDigits(ctx: Context[S], next: Context[S] => M[Int]): M[Int] =
      ctx.stream.readWhile(ctx.target, ctx.ptr, ctx.targetEnd, isDigit) <||| { readCount =>
        /* End of file reached. ReadDigits is always invoked "inside" the number.
         * The digits may be in the three parts: integer, decimal and exponent.
         * In all these cases the number is well-formed.
         */
        if readCount < 0 then {
          ctx.goTo(State.Eof)
          Monad.pure(ctx.charactersWritten())
        } else {
          ctx.ptr += readCount
          if ctx.endOfOutput() then
            Monad.pure(ctx.charactersWritten())
          else
            next(ctx)
        }
      }
  }


  /**
   * Checks if the character is a valid (json) digit. The character being digit
   * does not mean that the sequence of characters would be valid number. The
   * integer value part (and the only part) could not have leading zero(es).
   */
  def isDigit(char: Int): Boolean =
    '0' <= char && char <= '9'

  /**
   * Checks if the character is valid number start.
   */
  def isNumberStart(char: Int): Boolean =
    isNumberSign(char) || isDigit(char)


  /** Checks if the character is valid number sign. */
  def isNumberSign(char: Int): Boolean =
    char == '-'


  /** Checks if the character is a decimal separator. */
  def isDecimalSeparator(char: Int): Boolean =
    char == '.'


  /** Checks if the character is an exponent indicator. */
  def isExponentIndicator(char: Int): Boolean =
    char == 'E' || char == 'e'


  /** Checks if the character is an exponent sign. */
  def isExponentSign(char: Int): Boolean =
    char == '+' || char == '-'
}
