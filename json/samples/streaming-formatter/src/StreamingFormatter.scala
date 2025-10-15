package io.github.maxkar
package json.sample.formatter.streaming

import json.parser.Whitespaces
import json.parser.Literals
import json.parser.{Strings => IStrings}
import json.parser.Numbers
import json.parser.Arrays
import json.parser.Objects
import json.parser.Values

import json.writer.Layout
import json.writer.{Strings => OStrings}

import fun.typeclass.Monad
import fun.instances.Unnest
import fun.instances.Unnest.given

import java.io.IOException

/** Streaming formatter for the JSON values. */
object StreamingFormatter {
  /** "Factory" for literals. */
  private object LiteralFactory extends Literals.Factory.RaiseParseErrors[Unnest, FormatterIO, Unit] {
    override def read(stream: FormatterIO, count: Int): Unnest[Unit] =
      Monad.pure(stream.copy(count))
  }


  /** Copy function for "true" values. */
  private val copyTrue = Literals.readTrue(LiteralFactory)
  /** Copy function for "false" values. */
  private val copyFalse = Literals.readFalse(LiteralFactory)
  /** Copy function for "null" values. */
  private val copyNull = Literals.readNull(LiteralFactory)


  /** "Factory" for strings. */
  private object StringFactory extends IStrings.Factory.RaiseParseErrors[Unnest, FormatterIO, Unit] {
    override type Context = Unit
    override def start(stream: FormatterIO, count: Int): Unnest[Unit] =
      Monad.pure(stream.copy(count))
    override def readWhile(stream: FormatterIO, context: Unit, predicate: Char => Boolean) : Unnest[Unit] =
      Monad.pure(stream.copyWhile(predicate))
    override def readEscape(stream: FormatterIO, context: Unit, count: Int, char: Char): Unnest[Unit] =
      OStrings.write(stream, char) >-| { stream.drop(count) }
    override def finish(stream: FormatterIO, context: Unit, count: Int): Unnest[Unit] =
      Monad.pure(stream.copy(count))
  }
  private val copyString = IStrings.read(StringFactory)


  /** "Factory" for numbers. */
  private object NumberFactory extends Numbers.Factory.RaiseParseErrors[Unnest, FormatterIO, Unit] {
    override type Context = Unit

    override def start(stream: FormatterIO): Unnest[Unit] = Monad.pure(())
    override def readSign(stream: FormatterIO, context: Unit, count: Int, sign: Char): Unnest[Unit] =
      Monad.pure(stream.copy(count))
    override def readIntegerDigits(stream: FormatterIO, context: Unit, predicate: Char => Boolean): Unnest[Unit] =
      Monad.pure(stream.copyWhile(predicate))
    override def readDecimalSeparator(stream: FormatterIO, context: Unit, count: Int, separator: Char): Unnest[Unit] =
      Monad.pure(stream.copy(count))
    override def readDecimalDigits(stream: FormatterIO, context: Unit, predicate: Char => Boolean): Unnest[Unit] =
      Monad.pure(stream.copyWhile(predicate))
    override def readExponentIndicator(stream: FormatterIO, context: Unit, count: Int, separator: Char): Unnest[Unit] =
      Monad.pure(stream.copy(count))
    override def readExponentSign(stream: FormatterIO, context: Unit, count: Int, separator: Char): Unnest[Unit] =
      Monad.pure(stream.copy(count))
    override def readExponentDigits(stream: FormatterIO, context: Unit, predicate: Char => Boolean): Unnest[Unit] =
      Monad.pure(stream.copyWhile(predicate))
    override def finish(stream: FormatterIO, context: Unit): Unnest[Unit] = Monad.pure(())
  }
  private val copyNumber = Numbers.read(NumberFactory)


  private final class ArrayFactory(layout: Layout[Unnest, FormatterIO]) extends Arrays.Factory.RaiseParseErrors[Unnest, FormatterIO, Unit] {
    private val arrayLayout = layout.arrayLayout
    private val nestedLayout = layout.nested

    final class Context { var first: Boolean = true }

    override def skipIgnorableWhitespaces(stream: FormatterIO): Unnest[Unit] =
      Monad.pure(stream.dropWhile(Whitespaces.isWhitespace))

    override def start(stream: FormatterIO, count: Int): Unnest[Context] =
      Monad.pure {
        stream.copy(count)
        new Context()
      }

    override def readValue(stream: FormatterIO, context: Context): Unnest[Unit] = {
      val isFirst = context.first
      context.first = false
      arrayLayout.beforeValue(stream, context.first) >=|| formatJson(stream, nestedLayout)
    }

    override def skipValueSeparator(stream: FormatterIO, context: Context, count: Int): Unnest[Unit] =
      arrayLayout.beforeElementSeparator(stream) >-| { stream.copy(count) }
    override def finish(stream: FormatterIO, context: Context, count: Int): Unnest[Unit] =
      arrayLayout.beforeArrayEnd(stream, context.first) >-| { stream.copy(count) }
  }
  def copyArray(stream: FormatterIO, layout: Layout[Unnest, FormatterIO]): Unnest[Unit] =
    Arrays.read(new ArrayFactory(layout))(stream)


  private final class ObjectFactory(layout: Layout[Unnest, FormatterIO]) extends Objects.Factory.RaiseParseErrors[Unnest, FormatterIO, Unit] {
    private val objectLayout = layout.objectLayout
    private val nestedLayout = layout.nested

    final class Context { var first: Boolean = true }
    override type Key = Unit

    override def skipIgnorableWhitespaces(stream: FormatterIO): Unnest[Unit] =
      Monad.pure(stream.dropWhile(Whitespaces.isWhitespace))
    override def start(stream: FormatterIO, count: Int): Unnest[Context] =
      Monad.pure {
        stream.copy(count)
        new Context()
      }
    override def readKey(stream: FormatterIO, context: Context): Unnest[Unit] = {
      val isFirst = context.first
      context.first = false
      objectLayout.beforeKey(stream, isFirst) >=|| copyString(stream)
    }
    override def skipKeyValueSeparator(stream: FormatterIO, context: Context, key: Unit, count: Int): Unnest[Unit] =
      objectLayout.beforeKeyValueSeparator(stream) >-| stream.copy(count)
    override def readValue(stream: FormatterIO, context: Context, key: Unit): Unnest[Unit] =
      objectLayout.beforeValue(stream) >=|| formatJson(stream, layout)
    override def skipEntrySeparator(stream: FormatterIO, context: Context, count: Int): Unnest[Unit] =
      objectLayout.beforeEntrySeparator(stream) >-| stream.copy(count)
    override def finish(stream: FormatterIO, context: Context, count: Int): Unnest[Unit] =
      objectLayout.beforeObjectEnd(stream, context.first) >-| stream.copy(count)
  }
  def copyObject(stream: FormatterIO, layout: Layout[Unnest, FormatterIO]): Unnest[Unit] =
    Objects.read(new ObjectFactory(layout))(stream)


  private class JsonFactory(layout: Layout[Unnest, FormatterIO]) extends Values.Factory.RaiseParseErrors[Unnest, FormatterIO, Unit] {
    override def readTrue(stream: FormatterIO): Unnest[Unit] = copyTrue(stream)
    override def readFalse(stream: FormatterIO): Unnest[Unit] = copyFalse(stream)
    override def readNull(stream: FormatterIO): Unnest[Unit] = copyNull(stream)
    override def readString(stream: FormatterIO): Unnest[Unit] = copyString(stream)
    override def readNumber(stream: FormatterIO): Unnest[Unit] = copyNumber(stream)
    override def readArray(stream: FormatterIO): Unnest[Unit] = copyArray(stream, layout)
    override def readObject(stream: FormatterIO): Unnest[Unit] = copyObject(stream, layout)
  }


  def formatJson(stream: FormatterIO, layout: Layout[Unnest, FormatterIO]): Unnest[Unit] =
    Values.read(new JsonFactory(layout))(stream)

  def formatFully(stream: FormatterIO, layout: Layout[Unnest, FormatterIO]): Unit =
    Unnest.run {
      Values.readFully(new JsonFactory(layout))(stream)
    }


  /** Raises an exception with the given message. */
  private def raise[T](stream: FormatterIO, message: String): Unnest[T] =
    throw new IOException(s"${stream.getLocation()}: ${message}")
}
