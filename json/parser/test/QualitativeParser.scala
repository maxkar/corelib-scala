package io.github.maxkar
package json.parser

import fun.typeclass.Monad

/**
 * Qualitative parser definitions - only indicates if parsing of a given string was successfull or not.
 */
object QualitativeParser:
  /**
   * Parsing "monad" - a function from input string and position into new input position and outcome.
   */
  type Md[T] = (String, Int) => (Int, Option[T])

  /** Runs the "monad"/processor on the string. */
  def runMd[T](input: String, v: Md[T]): (Int, Option[T]) = v(input, 0)

  /** Monad implementation for MD. */
  given MdMonad: Monad[Md] with
    override def pure[T](v: T): Md[T] =
      (i, pos) => (pos, Some(v))

    override def bind[S, R](v: Md[S], fn: S => Md[R]): Md[R] =
      (i, pos) =>
        v(i, pos) match
          case (npos, None) => (npos, None)
          case (npos, Some(v)) => fn(v)(i, npos)
  end MdMonad


  /** Very basic implementation of input over the Md. */
  object Input extends input.BasicInput[Md]:
    override def statefulScan[S](initialState: S, step: (CharSequence, S) => (input.ConsumerStatus, S)): Md[S] =
      (i, pos) =>
        val (consumerStatus, newState) = step(i.substring(pos), initialState)
        val npos =
          consumerStatus match {
            case input.ConsumerStatus.NeedMoreInput => i.length
            case input.ConsumerStatus.Finished(chrs) => pos + chrs
          }
        (npos, Some(newState))
  end Input


  /** Creates a "failure" value. */
  def fail[T]: Md[T] = (i, pos) => (pos, None)


  /** A primitive (qualitative - parses/not parses) json factory. */
  object Factory extends factory.JsonFactory[Md, Unit]:
    override def badValue: Md[Unit] = fail
    override val trueFactory =
      new factory.TermFactory[Md, Unit]:
        override type State = Unit
        override def begin: Md[Unit] = Monad.pure(())
        override def badInput(state: Unit, expected: String, offest: Int): Md[Unit] = fail
        override def unexpectedEnd(state: Unit, expected: String, offest: Int): Md[Unit] = fail
        override def end(state: Unit): Md[Unit] = Monad.pure(())
      end new

    override val falseFactory = trueFactory
    override val nullFactory = trueFactory

    override val stringFactory =
      new factory.StringFactory[Md, Unit]:
        override type State = Unit
        override def begin: Md[Unit] = Monad.pure(())
        override def update(st: Unit, input: CharSequence): (Unit, Boolean) = ((), true)
        override def update(st: Unit, input: Char): (Unit, Boolean) = ((), true)
        override def end(st: Unit): Md[Unit] = Monad.pure(())
        override def badCharacter(s: Unit): Md[Unit] = fail
        override def badEscape(s: Unit): Md[Unit] = fail
        override def badUnicodeEscape(s: Unit): Md[Unit] = fail
        override def badUnicodeEscape(s: Unit, c0: Char): Md[Unit] = fail
        override def badUnicodeEscape(s: Unit, c0: Char, c1: Char): Md[Unit] = fail
        override def badUnicodeEscape(s: Unit, c0: Char, c1: Char, c2: Char): Md[Unit] = fail
        override def unterminatedString(s: Unit): Md[Unit] = fail
      end new


    override val arrayFactory =
      new factory.ArrayFactory[Md, Unit]:
        override type State = Unit

        override def begin: Md[Unit] = Monad.pure(())
        override def update(st: Unit, nextVal: Unit): (Unit, Boolean) = ((), true)
        override def end(st: Unit): Md[Unit] = Monad.pure(())

        override def badArrayContinuation(st: Unit): Md[Unit] = fail
      end new


    override val objectFactory =
      new factory.ObjectFactory[Md, Unit]:
        override type State = Unit
        override type Key = Unit

        override val keyFactory = stringFactory

        override def begin: Md[Unit] = Monad.pure(())
        override def update(st: Unit, key: Unit, value: Unit): (Unit, Boolean) = ((), true)
        override def end(st: Unit): Md[Unit] = Monad.pure(())

        override def badKeyStart(state: Unit): Md[Unit] = fail

        override def badKeyValueSeparator(state: Unit, key: Unit): Md[Unit] = fail

        override def badObjectContinuation(state: Unit): Md[Unit] = fail
      end new


    override val numberFactory =
      new factory.NumberFactory[Md, Unit]:
        override type State = Unit
        override def begin: Md[Unit] = Monad.pure(())
        override def update(st: Unit, input: CharSequence): (Unit, Boolean) = ((), true)
        override def end(st: Unit): Md[Unit] = Monad.pure(())

        override def digitsAfterLeadingZero(st: Unit): Md[Unit] = fail
        override def missingIntDigits(st: Unit): Md[Unit] = fail
        override def missingFractionalDigits(st: Unit): Md[Unit] = fail
        override def missingExponentDigits(st: Unit): Md[Unit] = fail
      end new
  end Factory
end QualitativeParser
