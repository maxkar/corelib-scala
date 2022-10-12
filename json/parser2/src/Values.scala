package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Functor
import fun.Monad

/** Generic value-related functionality. */
object Values:
  /** Error encoding for values. */
  trait Errors[M[_]]:
    /**
     * Handles a situation where value is expected but none is given.
     */
    def illegalValue[T](): M[T]
  end Errors


  /** All errors combined. */
  trait AllErrors[M[_]]:
    /** Value errors handlers. */
    given valueErrors: Errors[M]
    /** Literal error handlers. */
    given literalErrors: Literals.Errors[M]
    /** Number error handlers. */
    given numberErrors: Numbers.Errors[M]
    /** String error handlers. */
    given stringErrors: Strings.Errors[M]
    /** String error handlers. */
    given stringStartErrors: Strings.StartErrors[M]
    /** Array error handlers. */
    given arrayErrors: Arrays.Errors[M]
    /** Object error handlers. */
    given objectErrors: Objects.Errors[M]
  end AllErrors


  /**
   * Callback for specific value or value type being observed in the stream.
   * @tparam T type of the value returned by the callback.
   */
  trait ValueCallback[T]:
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
  end ValueCallback


  /**
   * A very simple builder of the JSON model.
   * @tparam T type of the JSON model element.
   */
  trait SimpleBuilder[T]:
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
  end SimpleBuilder



  /**
   * Classifies the value or value type by its first character.
   * @param char the (first) character of the object.
   * @param callback handler for each particular look-ahead type.
   * @param noMatch handler for "no match" value where character is not a valid
   *   first character of **any** json value.
   * @return one of the values returned by the `callback` or `noMatch`.
   */
  def expectedValue[T](char: Char, callback: ValueCallback[T], noMatch: => T): T =
    char match
      case 't' => callback.onTrue()
      case 'f' => callback.onFalse()
      case 'n' => callback.onNull()
      case '"' => callback.onString()
      case '{' => callback.onObject()
      case '[' => callback.onArray()
      case '-' => callback.onNull()
      case x if '0' <= x && x <= '9' => callback.onNumber()
      case _ => noMatch
    end match
  end expectedValue


  /**
   * Looks at thes stream and invokes the appropriate callback for the look-ahead token.
   * @param stream stream to look at.
   * @param callback handler for each particular look-ahead type.
   * @param noMatch handler for "no match" value where character is not a valid
   *   first character of **any** json value.
   * @return one of the values returned by the `callback` or `noMatch`.
   */
  def expectedValue[T, M[_]: Functor](stream: CharacterStream[M], callback: ValueCallback[T], noMatch: => T): M[T] =
    stream.peek(1) map { lookAhead =>
      if lookAhead.length() <= 0 then
        noMatch
      else
        expectedValue(lookAhead.charAt(0), callback, noMatch)
    }
  end expectedValue


  /**
   * Reads a simple JSON model. The stream skips leading (but not trailing) whitespaces
   * and then constructs model from the underlying JSON representation.
   *
   * @param model model builder - how to build JSON representation from elements.
   * @param stream data stream to read.
   */
  def readSimple[T, M[_]: Monad: AllErrors](
        model: SimpleBuilder[T],
        stream: CharacterStream[M],
      ): M[T] =
    val errs = implicitly[AllErrors[M]]
    import errs.given

    object reader extends ValueCallback[M[T]]:
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
          readKey = Strings.readAll,
          readValue = readValue,
          stream = stream
        ) map model.fromObject

      /** Reads a value. */
      def readValue(stream: CharacterStream[M]): M[T] =
        Monad.flatten(expectedValue(stream, this, errs.valueErrors.illegalValue()))
    end reader

    Whitespaces.skipAll(stream) flatMap { _ => reader.readValue(stream) }
  end readSimple
end Values
