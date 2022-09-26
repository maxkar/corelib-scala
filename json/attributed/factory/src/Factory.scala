package io.github.maxkar
package json.attr.factory

import json.attr.Json
import json.parser.factory.JsonFactory
import json.parser.factory.TermFactory
import json.parser.factory.StringFactory
import json.parser.factory.NumberFactory
import json.parser.factory.ArrayFactory
import json.parser.factory.ObjectFactory

import fun.Monad

/**
 * Json object/value factory compatible with the standard parser API.
 * @tparam M calculation monad.
 * @tparam L location representation in the source.
 * @tparam A type of the json attributes.
 * @param location location monad - a way to get current source location.
 * @param locationToString tranformer from location to string (used in error messages).
 * @param fail failure factor - encodes/transfers text message to failure execution.
 * @param makeAttrs function to convert (start, end) location pair into attributes.
 *   The function may choose what to encode (some scenarios may only need start location,
 *   others may need both start and end locations). It may also encode additional information
 *   like source file name.
 */
final class Factory[M[+_]: Monad, L, A](
      location: M[L],
      locationToString: L => String,
      fail: String => M[Nothing],
      makeAttrs: (L, L) => A)
    extends JsonFactory[M, Json[A]]:


  /**
   * Implementation of term parser.
   * @param fn function used to create value from attributes.
   */
  private final class TermFactoryImpl(fn: A => Json[A])
      extends TermFactory[M, Json[A]]:
    override type State = L

    override def begin: M[State] = location
    override def end(startLocation: State): M[Json[A]] =
      for {
        endLocation <- location
      } yield
        fn(makeAttrs(startLocation, endLocation))

    override def badInput(startLocation: State, expectedTerm: String, offset: Int): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(startLocation)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Invalid/unexpected character of the ${expectedTerm} literal started at ${startStr}"
        fail(msg)
      }

    override def unexpectedEnd(startLocation: State, expectedTerm: String, offset: Int): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(startLocation)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected end of file inside ${expectedTerm} literal started at ${startStr}"
        fail(msg)
      }
  end TermFactoryImpl


  /**
   * Implementation of the string factory. There are slightly different contexts and usage so
   * this parametrized class.
   *
   * @tparam T expected result type.
   * @param convert function to make the expected type.
   */
  private final class StringFactoryImpl[T](convert: (String, L, L) => T) extends StringFactory[M, T]:
    override type State = (L, StringBuilder)

    override def begin: M[State] =
      for loc <- location yield (loc, new StringBuilder())

    override def update(state: State, input: CharSequence): (State, Boolean) = {
      state._2.append(input)
      (state, true)
    }

    override def update(state: State, input: Char): (State, Boolean) = {
      state._2.append(input)
      (state, true)
    }

    override def end(state: State): M[T] =
      for {
        endLocation <- location
      } yield
        convert(state._2.toString, state._1, endLocation)


    override def badEscape(state: State): M[T] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected escaped character in string started at ${startStr}"
        fail(msg)
      }

    override def badUnicodeEscape(state: State): M[T] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected unicode escape letter in string started at ${startStr}"
        fail(msg)
      }

    override def badUnicodeEscape(state: State, c1: Char): M[T] =
      badUnicodeEscape(state)

    override def badUnicodeEscape(state: State, c1: Char, c2: Char): M[T] =
      badUnicodeEscape(state)

    override def badUnicodeEscape(state: State, c1: Char, c2: Char, c3: Char): M[T] =
      badUnicodeEscape(state)

    override def badCharacter(state: State): M[T] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected/illegal character in string started at ${startStr}"
        fail(msg)
      }

    override def unterminatedString(state: State): M[T] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unclosed string literal for string started at ${startStr}"
        fail(msg)
      }
  end StringFactoryImpl


  override val nullFactory: TermFactory[M, Json[A]] = new TermFactoryImpl(Json.Null.apply)
  override val trueFactory: TermFactory[M, Json[A]] = new TermFactoryImpl(Json.True.apply)
  override val falseFactory: TermFactory[M, Json[A]] = new TermFactoryImpl(Json.False.apply)


  override val stringFactory = new StringFactoryImpl[Json[A]]((value, start, end) => Json.String(value, makeAttrs(start, end)))


  override object numberFactory extends NumberFactory[M, Json[A]]:
    override type State = (L, StringBuilder)

    override def begin: M[State] =
      for loc <- location yield (loc, new StringBuilder())

    override def update(state: State, input: CharSequence): (State, Boolean) = {
      state._2.append(input)
      (state, true)
    }

    override def end(state: State): M[Json[A]] =
      for {
        endLocation <- location
      } yield
        Json.Number(state._2.toString, makeAttrs(state._1, endLocation))

    override def missingIntDigits(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Missing integer part inside numeric literal started at ${startStr}"
        fail(msg)
      }

    override def missingFractionalDigits(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Missing fractional digits inside numeric literal started at ${startStr}"
        fail(msg)
      }


    override def missingExponentDigits(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Missing exponent digits inside numeric literal started at ${startStr}"
        fail(msg)
      }

    override def digitsAfterLeadingZero(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Illegal digits after leading 0 in the numeric literal started at ${startStr}"
        fail(msg)
      }
  end numberFactory


  override object arrayFactory extends ArrayFactory[M, Json[A]]:
    import scala.collection.mutable.ArrayBuffer

    override type State = (L, ArrayBuffer[Json[A]])

    override def begin: M[State] =
      for loc <- location yield (loc, new ArrayBuffer[Json[A]]())

    override def update(state: State, input: Json[A]): (State, Boolean) = {
      state._2.append(input)
      (state, true)
    }

    override def end(state: State): M[Json[A]] =
      for {
        endLocation <- location
      } yield
        Json.Array(state._2.toSeq, makeAttrs(state._1, endLocation))

    override def badArrayContinuation(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected character inside array literal started at ${startStr}, comma or end of array is expected"
        fail(msg)
      }
  end arrayFactory


  override object objectFactory extends ObjectFactory[M, Json[A]]:
    import scala.collection.mutable.HashMap

    override type Key = (String, L, L)

    /* State - initial location, current data, optional _duplicate_ key. */
    override type State = (L, HashMap[String, (Key, Json[A])], Option[Key])


    override val keyFactory: StringFactory[M, Key] =
      new StringFactoryImpl[Key](
        (value, start, end) => (value, start, end)
      )

    override def begin: M[State] =
      for loc <- location yield (loc, new HashMap(), None)


    override def update(state: State, key: Key, value: Json[A]): (State, Boolean) =
      if state._2.contains(key._1)
      then ((state._1, state._2, Some(key)), false)
      else
        state._2.put(key._1, (key, value))
        (state, true)
    end update


    override def end(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        state._3 match
          case Some((key, dupeStart, _)) =>
            val startStr = locationToString(state._1)
            val endStr = locationToString(dupeStart)
            val prevOcc = locationToString(state._2(key)._1._2)
            val msg = s"${endStr}: Duplicate key ${key} inside object started at ${startStr}, previous occurence at ${prevOcc}"
            fail(msg)
          case None =>
            val objAttrs = makeAttrs(state._1, endLocation)
            val elems = state._2.view.mapValues {
              /* Unfortunately, only one level of untupling works automatically. */
              case ((key, start, end), v) => Json.ObjectEntry(key, makeAttrs(start, end), v)
            }
            Monad.pure(Json.Object(elems.toMap, objAttrs))
        end match
      }

    override def badKeyStart(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected character inside object literal started at ${startStr}, key expected"
        fail(msg)
      }


    override def badObjectContinuation(state: State): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected character inside object literal started at ${startStr}, comma or end of object is expected"
        fail(msg)
      }


    override def badKeyValueSeparator(state: State, key: Key): M[Json[A]] =
      location.flatMap { endLocation =>
        val startStr = locationToString(state._1)
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected character inside object literal started at ${startStr}, colon is expected"
        fail(msg)
      }
  end objectFactory


  override def badValue: M[Json[A]] =
      location.flatMap { endLocation =>
        val endStr = locationToString(endLocation)
        val msg = s"${endStr}: Unexpected start of JSON value"
        fail(msg)
      }
end Factory
