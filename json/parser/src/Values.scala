package io.github.maxkar
package json.parser

import fun.typeclass.Functor
import fun.typeclass.Monad

import text.input.LookAheadStream

/** Generic value-related functionality. */
object Values {
  /**
   * Error handler for general value errors.
   * @tparam M execution (monad).
   * @tparam S type of the stream (i.e. "context") required by the error generation.
   */
  trait Errors[M[_], -S] {
    /**
     * Invoked when JSON value is expected but the next input character
     * (if present) is not valid start of any JSON value.
     * Stream position is before what was expected to be a first character
     * of the input value.
     */
    def illegalValue[T](stream: S): M[T]
  }


  /**
   * All errors combined.
   * @tparam M execution monad.
   * @tparam S type of the stream (context) used by the computation.
   */
  trait AllErrors[M[_], -S] {
    /** Value errors handlers. */
    given valueErrors: Errors[M, S]
    /** Literal error handlers. */
    given literalErrors: Literals.Errors[M, S]
    /** Number error handlers. */
    given numberErrors: Numbers.Errors[M, S]
    /** String error handlers. */
    given stringErrors: Strings.Errors[M, S]
    /** Array error handlers. */
    given arrayErrors: Arrays.Errors[M, S]
    /** Object error handlers. */
    given objectErrors: Objects.Errors[M, S]
  }


  /**
   * Callback for specific value or value type being observed in the stream.
   * @tparam T type of the value returned by the callback.
   */
  trait ValueCallback[T] {
    /** The value is the `true` literal. */
    def onTrue(): T
    /** The value is the `false` literal. */
    def onFalse(): T
    /** The value is the `null` literal. */
    def onNull(): T
    /** The next value is number. */
    def onNumber(): T
    /** The next vaule is string. */
    def onString(): T
    /** The next value is array. */
    def onArray(): T
    /** The next vaule is object. */
    def onObject(): T
  }


  /**
   * Very simple builder of the JSON model.
   * @tparam T type of the JSON model element.
   */
  trait SimpleBuilder[T] {
    /** Creates a new element from the boolean value. */
    def fromBoolean(v: Boolean): T
    /** Encodes explicit `null` value. */
    def fromNull(): T
    /**
     * Encodes numeric value.
     * @param repr JSON number representation that occured in the input.
     */
    def fromNumber(repr: String): T
    /** Encodes string value. */
    def fromString(value: String): T
    /** Encodes array value. */
    def fromArray(items: Seq[T]): T
    /** Encodes object value. */
    def fromObject(items: Map[String, T]): T
  }



  /**
   * Classifies the value or value type by its first character.
   * @param char the (first) character of the object.
   * @param callback handler for each particular look-ahead type.
   * @param noMatch handler for "no match" value where character is not a valid
   *   first character of **any** json value.
   * @return one of the values returned by the `callback` or `noMatch`.
   */
  def expectedValue[T](char: Char, callback: ValueCallback[T], noMatch: => T): T =
    char match {
      case 't' => callback.onTrue()
      case 'f' => callback.onFalse()
      case 'n' => callback.onNull()
      case '"' => callback.onString()
      case '{' => callback.onObject()
      case '[' => callback.onArray()
      case '-' => callback.onNumber()
      case x if '0' <= x && x <= '9' => callback.onNumber()
      case _ => noMatch
    }


  /**
   * Looks at thes stream and invokes the appropriate callback for the look-ahead token.
   * @param stream stream to look at.
   * @param callback handler for each particular look-ahead type.
   * @param noMatch handler for "no match" value where character is not a valid
   *   first character of **any** json value.
   * @return one of the values returned by the `callback` or `noMatch`.
   */
  def expectedValue[T, M[_]: Monad, S <: LookAheadStream[M]](
          stream: S,
          callback: ValueCallback[M[T]],
        )(implicit
          errs: Errors[M, S],
        ) : M[T] =
    stream.peek(1) flatMap { lookAhead =>
      if lookAhead.length() <= 0 then
        errs.illegalValue(stream)
      else
        expectedValue(lookAhead.charAt(0), callback, errs.illegalValue(stream))
    }


  /**
   * Reads a simple JSON model. The stream skips leading (but not trailing) whitespaces
   * and then constructs model from the underlying JSON representation.
   *
   * @param model model builder - how to build JSON representation from elements.
   * @param stream data stream to read.
   */
  def readSimple[T, M[_]: Monad, S <: LookAheadStream[M]](
        model: SimpleBuilder[T],
        stream: S,
      )(using
        errs: AllErrors[M, S]
      ): M[T] = {
    import errs.given

    object reader extends ValueCallback[M[T]] {
      override def onTrue(): M[T] =
        Literals.readTrue(stream) map { _ => model.fromBoolean(true) }

      override def onFalse(): M[T] =
        Literals.readFalse(stream) map { _ => model.fromBoolean(false) }

      override def onNull(): M[T] =
        Literals.readNull(stream) map { _ => model.fromNull() }

      override def onNumber(): M[T] =
        Numbers.readAll(stream) map model.fromNumber

      override def onString(): M[T] =
        Strings.readAll(stream) map model.fromString

      override def onArray(): M[T] =
        Arrays.readAll(
          skipWhitespaces = Whitespaces.skipAll[M],
          readValue = readValue,
          stream = stream
        ) map model.fromArray

      override def onObject(): M[T] =
        Objects.readAll(
          skipWhitespaces = Whitespaces.skipAll[M],
          readKey = Strings.readAll[M, S],
          readValue = readValue,
          stream = stream
        ) map model.fromObject

      /** Reads a value. */
      def readValue(stream: S): M[T] =
        expectedValue(stream, this)
    }

    Whitespaces.skipAll(stream) flatMap { _ => reader.readValue(stream) }
  }
}
