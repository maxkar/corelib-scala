package io.github.maxkar
package json.parser

import fun.Monad

/**
 * Error-related utilities.
 */
object Errors:

  /**
   * Very simple error handler that just "raises" the error using
   * the provided error message and the context (stream) where it happened.
   */
  trait SimpleHandler[M[_], S]:
    /**
      * Raises (aka encodes) the error.
      *
      * @param stream stream where the error has happened. May be used to get
      *   the context (like location).
      * @param message human-readable message description.
      * @return encoded error.
      */
    def raise[T](stream: S, message: String): M[T]
  end SimpleHandler



  /**
   * Creates a set of json parsing handlers that just raise one standard
   * error with human-readable message.
   * @param handler the handler that knows how to encode the errorm into the
   *   execution monad M. It may also enrich the message with the error context
   *   (like location) from the input stream.
   */
  def simple[M[_]: Monad, S <: CharacterStream[M]](handler: SimpleHandler[M, S]): Values.AllErrors[M, S] =
    new Values.AllErrors[M, S]:
      /** Returns "context" of the stream that could be used in the messages. */
      private def getContext(seq: CharSequence, maxLength: Int = 7): String =
        val len = Math.min(maxLength, seq.length())
        seq.subSequence(0, len).toString()
      end getContext

      /**
       * Universal handler for errors that fetches the context and provides
       * a EOF or non-EOF message with some input context.
       *
       * @param stream stream to report the error on.
       * @param entityName what was being read (used for End-Of-File message).
       * @param buildMessage function to build message based on the "look-ahead" data
       *   (that may be included into the message).
       * @param contextLength number of characters to look for and use in the
       *   message context.
       */
      private def message[T](
            stream: S,
            entityName: String,
            buildMessage: String => String,
            contextLength: Int = 7
          ): M[T] =
        stream.peek(contextLength) flatMap { lookAhead =>
          val msg =
            if lookAhead.length() <= 0 then
              s"Unexpected end of stream, expected ${entityName}"
            else
              getContext(lookAhead, contextLength)
          handler.raise(stream, msg)
        }


      override given valueErrors: Values.Errors[M, S] with
        override def illegalValue[T](stream: S): M[T] =
          message(
            stream,
            "value",
            la => s"Unexpected start of json value '${la}'"
          )
      end valueErrors


      override given literalErrors: Literals.Errors[M, S] with
        override def badLiteral(expected: String, stream: S): M[Unit] =
          message(
            stream,
            s"Literal ${expected}",
            la => s"Invalid literal ${expected}, got '${la}'",
            expected.length()
          )
      end literalErrors


      override given numberErrors: Numbers.Errors[M, S] with
        override def missingIntegerDigits[T](stream: S): M[T] =
          message(
            stream,
            "integer digits",
            la => s"Invalid number format, integer part is expected, got '${la}'"
          )

        override def leadingIntegerZero[T](stream: S): M[T] =
          stream.peek(2) flatMap { lookAhead =>
            handler.raise(stream, s"Leading zero is not allowed in json numbers, got '${getContext(lookAhead, 2)}'")
          }

        override def missingDecimalDigits[T](stream: S): M[T] =
          message(
            stream,
            "decimal digits",
            la => s"Invalid number format, decimal digits are expected, got '${la}'"
          )

        override def missingExponentDigits[T](stream: S): M[T] =
          message(
            stream,
            "exponent digits",
            la => s"Invalid number format, exponent digits are expected, got '${la}'"
          )
      end numberErrors


      override given stringErrors: Strings.Errors[M, S] with
        override def illegalStringStart[T](stream: S): M[T] =
          message(
            stream,
            "string",
            la => s"Invalid string start, expected '\"' but got '${la.charAt(0)}'",
            contextLength = 1,
          )

        override def invalidEscapeCharacter[T](stream: S): M[T] =
          stream.peek(2) flatMap { lookAhead =>
            handler.raise(stream, s"Invalid escape sequence, '${getContext(lookAhead, 2)}'")
          }

        override def invalidUnicodeEscape[T](stream: S): M[T] =
          stream.peek(6) flatMap { lookAhead =>
            handler.raise(stream, s"Invalid unicode escape sequence, '${getContext(lookAhead, 6)}'")
          }

        override def invalidCharacter[T](stream: S): M[T] =
          stream.peek(1) flatMap { lookAhead =>
            handler.raise(stream, s"Invalid character, code='u${lookAhead.charAt(0).toInt.toHexString}'")
          }

        override def unterminatedString[T](stream: S): M[T] =
          handler.raise(stream, "Unexpected end of stream inside string literal")
      end stringErrors


      override given arrayErrors: Arrays.Errors[M, S] with
        override def invalidArrayStart[T](stream: S): M[T] =
          message(
            stream,
            "array",
            la => s"Invalid array start, expected '[' but got '${la.charAt(0)}'",
            contextLength = 1,
          )

        override def invalidArrayEnd[T](stream: S): M[T] =
          message(
            stream,
            "array end",
            la => s"Invalid array end (or value separator), expected ']' or ',' but got '${la.charAt(0)}'",
            contextLength = 1,
          )
      end arrayErrors


      override given objectErrors: Objects.Errors[M, S] with
        override def invalidObjectStart[T](stream: S): M[T] =
          message(
            stream,
            "object",
            la => s"Invalid object start, expected '{' but got '${la.charAt(0)}'",
            contextLength = 1,
          )

        override def invalidObjectEnd[T](stream: S): M[T] =
          message(
            stream,
            "object end",
            la => s"Invalid object end (or entity separator), expected '}' or ',' but got '${la.charAt(0)}'",
            contextLength = 1,
          )

        override def invalidKeyValueSeparator[T](stream: S): M[T] =
          message(
            stream,
            "key-value separator",
            la => s"Invalid key-value separator, expected ':' but got '${la.charAt(0)}'"
          )

      end objectErrors

    end new
  end simple
end Errors
