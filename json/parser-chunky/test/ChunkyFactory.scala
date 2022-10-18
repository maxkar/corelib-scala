package io.github.maxkar
package json.parser.chunky

import json.parser.factory

import fun.typeclass.Monad

/**
 * Json value factory that works with "chunky" parser.
 * @tparam M type of the parsing.
 * @param fail failure factory (defines the "monad" to use on failure).
 */
final class ChunkyFactory[M[+_]: Monad](fail: M[Nothing])
    extends factory.JsonFactory[M, Unit]:

  override def badValue: M[Unit] = fail

  override val trueFactory =
    new factory.TermFactory[M, Unit]:
      override type State = Unit
      override def begin: M[Unit] = Monad.pure(())
      override def badInput(state: Unit, expected: String, offest: Int): M[Unit] = fail
      override def unexpectedEnd(state: Unit, expected: String, offest: Int): M[Unit] = fail
      override def end(state: Unit): M[Unit] = Monad.pure(())
    end new

  override val falseFactory = trueFactory
  override val nullFactory = trueFactory

  override val stringFactory =
    new factory.StringFactory[M, Unit]:
      override type State = Unit
      override def begin: M[Unit] = Monad.pure(())
      override def update(st: Unit, input: CharSequence): (Unit, Boolean) = ((), true)
      override def update(st: Unit, input: Char): (Unit, Boolean) = ((), true)
      override def end(st: Unit): M[Unit] = Monad.pure(())
      override def badCharacter(s: Unit): M[Unit] = fail
      override def badEscape(s: Unit): M[Unit] = fail
      override def badUnicodeEscape(s: Unit): M[Unit] = fail
      override def badUnicodeEscape(s: Unit, c0: Char): M[Unit] = fail
      override def badUnicodeEscape(s: Unit, c0: Char, c1: Char): M[Unit] = fail
      override def badUnicodeEscape(s: Unit, c0: Char, c1: Char, c2: Char): M[Unit] = fail
      override def unterminatedString(s: Unit): M[Unit] = fail
    end new

  override val arrayFactory =
    new factory.ArrayFactory[M, Unit]:
      override type State = Unit

      override def begin: M[Unit] = Monad.pure(())
      override def update(st: Unit, nextVal: Unit): (Unit, Boolean) = ((), true)
      override def end(st: Unit): M[Unit] = Monad.pure(())

      override def badArrayContinuation(st: Unit): M[Unit] = fail
    end new


  override val objectFactory =
    new factory.ObjectFactory[M, Unit]:
      override type State = Unit
      override type Key = Unit

      override val keyFactory = stringFactory

      override def begin: M[Unit] = Monad.pure(())
      override def update(st: Unit, key: Unit, value: Unit): (Unit, Boolean) = ((), true)
      override def end(st: Unit): M[Unit] = Monad.pure(())

      override def badKeyStart(state: Unit): M[Unit] = fail

      override def badKeyValueSeparator(state: Unit, key: Unit): M[Unit] = fail

      override def badObjectContinuation(state: Unit): M[Unit] = fail
    end new

  override val numberFactory =
    new factory.NumberFactory[M, Unit]:
      override type State = Unit
      override def begin: M[Unit] = Monad.pure(())
      override def update(st: Unit, input: CharSequence): (Unit, Boolean) = ((), true)
      override def end(st: Unit): M[Unit] = Monad.pure(())

      override def digitsAfterLeadingZero(st: Unit): M[Unit] = fail
      override def missingIntDigits(st: Unit): M[Unit] = fail
      override def missingFractionalDigits(st: Unit): M[Unit] = fail
      override def missingExponentDigits(st: Unit): M[Unit] = fail
    end new
end ChunkyFactory
