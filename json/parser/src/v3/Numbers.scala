package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad

/** Number parsers. */
object Numbers {
  /**
   * Factory that creates a JSON representation `J` of a number
   * from the stream `S`.
   */
  trait Factory[M[_], S, J] {
    /** Context of the operation. */
    type Context

    /** Starts reading the number and creates the context. */
    def start(stream: S): M[Context]

    /** Consumes sign character from the input stream. */
    def consumeSign(stream: S, context: Context, count: Int, sign: Char): M[Unit]

    /** Consumes integer digits. */
    def consumeIntegerDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit]

    /** Handles a case where no integer part is present in the number. */
    def missingIntegerDigits(stream: S, context: Context): M[Unit]

    /** Processes a situation where a leading integer zero is present in JSON. */
    def leadingIntegerZero(stream: S, context: Context): M[Unit]

    /** Consumes the decimal separator. */
    def consumeDecimalSeparator(stream: S, context: Context, count: Int, separator: Char): M[Unit]

    /** Consumes decimal digits. */
    def consumeDecimalDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit]

    /** Handles a situation with missing decimal digits. */
    def missingDecimalDigits(stream: S, context: Context): M[Unit]

    /** Consumes exponent indicator. */
    def consumeExponentIndicator(stream: S, context: Context, count: Int, separator: Char): M[Unit]

    /** Consumes exponent sign. */
    def consumeExponentSign(stream: S, context: Context, count: Int, separator: Char): M[Unit]

    /** Consumes exponent digits. */
    def consumeExponentDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit]

    /** Handles a situation with missing exponent digits. */
    def missingExponentDigits(stream: S, context: Context): M[Unit]

    /** Finishes processing and constructs a number representation. */
    def finish(stream: S, context: Context): M[J]
  }


  /** Reads a number from the stream. */
  def read[M[_]: Monad, S: Peek.In[M], J](stream: S, factory: Factory[M, S, J]): M[J] =
    for
      ctx <- factory.start(stream)
      _ <- readSign(stream, factory, ctx)
      _ <- readIntegerDigits(stream, factory, ctx)
      _ <- readDecimal(stream, factory, ctx)
      _ <- readExponent(stream, factory, ctx)
      res <- factory.finish(stream, ctx)
    yield res


  /** Reads sign of the number. */
  private def readSign[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context
      ): M[Unit] =
    stream.peek(0) >=>> {
      case sgn@('+' | '-') => factory.consumeSign(stream, context, 1, sgn)
      case _ => Monad.pure(())
    }


  /** Reads integer digits. */
  private def readIntegerDigits[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context
      ): M[Unit] =
    stream.peek(0) >=>> {
      case '0' =>
        stream.peek(1) >=>> {
          case '0' => factory.leadingIntegerZero(stream, context)
          case _ => factory.consumeIntegerDigits(stream, context, isDigit)
        }
      case d if isDigitInt(d) => factory.consumeIntegerDigits(stream, context, isDigit)
      case other => factory.missingIntegerDigits(stream, context)
    }


  /** Reads the decimal part. */
  private def readDecimal[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context
      ): M[Unit] =
    stream.peek(0) >=>> {
      case '.' =>
        factory.consumeDecimalSeparator(stream, context, 1, '.') >=|| readDecimalDigits(stream, factory, context)
      case other => Monad.pure(())
    }


  /** Reads decimal digits. */
  private def readDecimalDigits[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context
      ): M[Unit] =
    stream.peek(0) >=>> { d =>
      if isDigitInt(d) then
        factory.consumeDecimalDigits(stream, context, isDigit)
      else
        factory.missingDecimalDigits(stream, context)
    }


  /** Reads the exponent part. */
  private def readExponent[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context
      ): M[Unit] =
    stream.peek(0) >=>> {
      case ind@('e' | 'E') =>
        factory.consumeExponentIndicator(stream, context, 1, ind) >=||
          readExponentSign(stream, factory, context) >=||
          readExponentDigits(stream, factory, context)
      case other => Monad.pure(())
    }


  /** Reads the exponent value. */
  private def readExponentSign[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context
      ): M[Unit] =
    stream.peek(0) >=>> {
      case sgn@('+' | '-') => factory.consumeExponentSign(stream, context, 1, sgn)
      case _ => Monad.pure(())
    }


  /** Reads exponent digits. */
  private def readExponentDigits[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        context: factory.Context
      ): M[Unit] =
    stream.peek(0) >=>> { d =>
      if isDigitInt(d) then
        factory.consumeExponentDigits(stream, context, isDigit)
      else
        factory.missingExponentDigits(stream, context)
    }


  /**
   * Checks if the character is a valid (json) digit. The character being digit
   * does not mean that the sequence of characters would be valid number. The
   * integer value part (and the only part) could not have leading zero(es).
   */
  def isDigit(char: Char): Boolean =
    '0' <= char && char <= '9'


  /**
   * Checks if the integer is a valid (json) digit. The character being digit
   * does not mean that the sequence of characters would be valid number. The
   * integer value part (and the only part) could not have leading zero(es).
   */
  def isDigitInt(char: Int): Boolean =
    '0' <= char && char <= '9'
}
