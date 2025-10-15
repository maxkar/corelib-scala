package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad
import fun.typeclass.Applicative

/** Number parsers. */
object Numbers {
  /**
   * Factory that creates a JSON representation `J` of a number
   * from the stream `S`.
   */
  trait Factory[M[_], -S, J] {
    /** Context of the operation. */
    type Context

    /** Starts reading the number and creates the context. */
    def start(stream: S): M[Context]

    /** Consumes sign character from the input stream. */
    def readSign(stream: S, context: Context, count: Int, sign: Char): M[Unit]

    /** Consumes integer digits. */
    def readIntegerDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit]

    /** Handles a case where no integer part is present in the number. */
    def missingIntegerDigits(stream: S, context: Context): M[Unit]

    /** Processes a situation where a leading integer zero is present in JSON. */
    def leadingIntegerZero(stream: S, context: Context): M[Unit]

    /** Consumes the decimal separator. */
    def readDecimalSeparator(stream: S, context: Context, count: Int, separator: Char): M[Unit]

    /** Consumes decimal digits. */
    def readDecimalDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit]

    /** Handles a situation with missing decimal digits. */
    def missingDecimalDigits(stream: S, context: Context): M[Unit]

    /** Consumes exponent indicator. */
    def readExponentIndicator(stream: S, context: Context, count: Int, separator: Char): M[Unit]

    /** Consumes exponent sign. */
    def readExponentSign(stream: S, context: Context, count: Int, separator: Char): M[Unit]

    /** Consumes exponent digits. */
    def readExponentDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit]

    /** Handles a situation with missing exponent digits. */
    def missingExponentDigits(stream: S, context: Context): M[Unit]

    /** Finishes processing and constructs a number representation. */
    def finish(stream: S, context: Context): M[J]
  }


  object Factory {
    abstract class RaiseParseErrors[M[_], -S: ParseError.In[M], J] extends Factory[M, S, J] {
      override final def missingIntegerDigits(stream: S, context: Context): M[Unit] =
        stream.parseError("Missing integer digits")
      override final def leadingIntegerZero(stream: S, context: Context): M[Unit] =
        stream.parseError("Leading zero is not allowed")
      override final def missingDecimalDigits(stream: S, context: Context): M[Unit] =
        stream.parseError("Missing decimal digits")
      override final def missingExponentDigits(stream: S, context: Context): M[Unit] =
        stream.parseError("Missing exponent digits")
    }

    abstract class Simple[M[_]: Applicative, -S: DefaultStream.In[M]: ParseError.In[M], J] extends RaiseParseErrors[M, S, J] {
      /** Creates a context. */
      def createContext(): Context

      /** Converts context to json value. */
      def createValue(context: Context): J

      /** Appends a simple character to the context. */
      def append(context: Context, chr: Char): Unit

      /** Appends a simple character to the context. */
      def appendWhile(context: Context, stream: S, predicate: Char => Boolean): M[Unit]

      override final def start(stream: S): M[Context] = Monad.pure(createContext())

      override final def readSign(stream: S, context: Context, count: Int, sign: Char): M[Unit] =
        stream.skip(1) >-| append(context, sign)

      override final def readIntegerDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        appendWhile(context, stream, predicate)

      override final def readDecimalSeparator(stream: S, context: Context, count: Int, separator: Char): M[Unit] =
        stream.skip(1) >-| append(context, separator)

      override final def readDecimalDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        appendWhile(context, stream, predicate)

      override final def readExponentIndicator(stream: S, context: Context, count: Int, separator: Char): M[Unit] =
        stream.skip(1) >-| append(context, separator)

      override final def readExponentSign(stream: S, context: Context, count: Int, separator: Char): M[Unit] =
        stream.skip(1) >-| append(context, separator)

      override final def readExponentDigits(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        appendWhile(context, stream, predicate)

      override final def finish(stream: S, context: Context): M[J] =
        Monad.pure(createValue(context))
    }


    /** Reader of a number as a simple string. */
    final class AsString[M[_]: Applicative, -S: DefaultStream.In[M]: ParseError.In[M]] extends Simple[M, S, String] {
      override type Context = StringBuilder

      override def createContext(): Context =
        new Context()
      override def append(context: Context, chr: Char): Unit =
        context += chr
      override def appendWhile(context: Context, stream: S, predicate: Char => Boolean): M[Unit] =
        stream.readWhile(context, predicate)
      override def createValue(context: StringBuilder): String =
        context.toString()
    }
  }


  /** Reads a number from the stream. */
  def read[M[_]: Monad, S: Peek.In[M], J](factory: Factory[M, S, J])(stream: S): M[J] =
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
      case sgn@('+' | '-') => factory.readSign(stream, context, 1, sgn)
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
          case _ => factory.readIntegerDigits(stream, context, isDigit)
        }
      case d if isDigitInt(d) => factory.readIntegerDigits(stream, context, isDigit)
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
        factory.readDecimalSeparator(stream, context, 1, '.') >=|| readDecimalDigits(stream, factory, context)
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
        factory.readDecimalDigits(stream, context, isDigit)
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
        factory.readExponentIndicator(stream, context, 1, ind) >=||
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
      case sgn@('+' | '-') => factory.readExponentSign(stream, context, 1, sgn)
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
        factory.readExponentDigits(stream, context, isDigit)
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
