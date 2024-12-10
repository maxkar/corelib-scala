package io.github.maxkar
package json.sample.formatter.streaming

import fun.typeclass.Monad

import text.input.LookAheadStream
import text.output.{Stream => OutStream}

import json.parser
import json.writer

/**
 * Base class for all formatters. Contains shared functionality like reading and
 * writing primitives.
 */
final class JsonFormatter[M[_]: Monad, S <: LookAheadStream[M]](
      input: S,
      output: OutStream[M],
      indent: Indent[M],
      errs: parser.Values.AllErrors[M, S],
      eofErrors: parser.EndOfFile.Errors[M, S]
    )
    extends parser.Values.ValueCallback[M[Unit]] {
  import errs.given


  /** Copies data from input into output with formatting. */
  def copy(): M[Unit] = {
    given parser.EndOfFile.Errors[M, S] = eofErrors

    for
      _ <- parser.Whitespaces.skipAll(input)
      _ <- copyValue()
      _ <- parser.Whitespaces.skipAll(input)
      res <- parser.EndOfFile.expectImmediately(input)
    yield res
  }

  /**
   * Copies single value from input into output. Expected to be called in the position
   * "just before" value (no whitespaces).
   */
  private def copyValue(): M[Unit] =
    parser.Values.expectedValue(input, this)


  override def onTrue(): M[Unit] =
    parser.Literals.readTrue(input).flatMap { _ =>
      writer.Literals.writeTrue(output)
    }

  override def onFalse(): M[Unit] =
    parser.Literals.readFalse(input).flatMap { _ =>
      writer.Literals.writeFalse(output)
    }

  override def onNull(): M[Unit] =
    parser.Literals.readNull(input).flatMap { _ =>
      writer.Literals.writeNull(output)
    }


  override def onNumber(): M[Unit] =
    for
      (digits, nextState) <- parser.Numbers.startParsing(input)
      _ <- output.write(digits)
      res <- continueNumber(nextState)
    yield
      res


  /** Continue reading number from the given state. */
  private def continueNumber(state: parser.Numbers.ParsingContinuation): M[Unit] =
    if state == null then
      Monad.pure(())
    else {
      for
        (digits, nextState) <- state.continue(input)
        _ <- output.write(digits)
        res <- continueNumber(nextState)
      yield
        res
    }


  /** Copies string from the input stream into the output stream. */
  override def onString(): M[Unit] =
    for
      (firstChunk, hasMore) <- parser.Strings.startString(input)
      _ <- writer.Strings.writeStringBoundary(output)
      _ <- writer.Strings.writeStringContent(firstChunk, output)
      res <- continueString(hasMore)
    yield res


  /** Continues copying string from the input into the output. */
  private def continueString(hasMore: Boolean): M[Unit] =
    if hasMore then
      for
        (nextChunk, hasMore) <- parser.Strings.continueString(input)
        _ <- writer.Strings.writeStringContent(nextChunk, output)
        res <- continueString(hasMore)
      yield res
    else
      writer.Strings.writeStringBoundary(output)


  override def onArray(): M[Unit] =
    for
      _ <- parser.Arrays.readArrayStart(input)
      _ <- parser.Whitespaces.skipAll(input)
      hasValues <- parser.Arrays.hasFirstValue(input)
      _ <- writer.Arrays.writeArrayStart(output)
      res <-
        if hasValues then
          copyNonEmptyArray()
        else
          writer.Arrays.writeArrayEnd(output)
    yield res


  /** Copies non-empty array. */
  private def copyNonEmptyArray(): M[Unit] =
    for
      _ <- indent.wrapAndIncrease(output)
      _ <- copyValue()
      _ <- parser.Whitespaces.skipAll(input)
      hasMore <- parser.Arrays.hasNextValue(input)
      res <- continueNonEmptyArray(hasMore)
    yield res


  /** Continues copying non-empty array. */
  private def continueNonEmptyArray(hasMore: Boolean): M[Unit] =
    if hasMore then
      for
        _ <- parser.Whitespaces.skipAll(input)
        _ <- writer.Arrays.writeArraySeparator(output)
        _ <- indent.wrapAndIndent(output)
        _ <- copyValue()
        _ <- parser.Whitespaces.skipAll(input)
        hasMore <- parser.Arrays.hasNextValue(input)
        res <- continueNonEmptyArray(hasMore)
      yield res
    else
      for
        _ <- indent.wrapAndDecrease(output)
        res <- writer.Arrays.writeArrayEnd(output)
      yield res


  override def onObject(): M[Unit] =
    for
      _ <- parser.Objects.readObjectStart(input)
      _ <- parser.Whitespaces.skipAll(input)
      hasEntries <- parser.Objects.hasFirstValue(input)
      _ <- writer.Objects.writeObjectStart(output)
      res <-
        if hasEntries then
          copyNonEmptyObject()
        else
          writer.Objects.writeObjectEnd(output)
    yield res


  /** Copies contents of non-empty object. */
  private def copyNonEmptyObject(): M[Unit] =
    for
      _ <- indent.wrapAndIncrease(output)
      _ <- copyKeyValuePair()
      hasNext <- parser.Objects.hasNextValue(input)
      res <- continueNonEmptyObject(hasNext)
    yield res


  /** Continues copying non-empty object. */
  private def continueNonEmptyObject(hasNext: Boolean): M[Unit] =
    if hasNext then
      for
        _ <- writer.Objects.writeElementSeparator(output)
        _ <- indent.wrapAndIndent(output)
        _ <- copyKeyValuePair()
        hasNext <- parser.Objects.hasNextValue(input)
        res <- continueNonEmptyObject(hasNext)
      yield res
    else
      for
        _ <- indent.wrapAndDecrease(output)
        res <- writer.Objects.writeObjectEnd(output)
      yield res


  /** Copies key-value pair in the object. */
  private def copyKeyValuePair(): M[Unit] =
    for
      _ <- parser.Whitespaces.skipAll(input)
      _ <- onString()
      _ <- parser.Whitespaces.skipAll(input)
      _ <- parser.Objects.readKeyValueSeparator(input)
      _ <- writer.Objects.writeKeyValueSeparator(output)
      _ <- indent.indentKeyValuePair(output)
      _ <- parser.Whitespaces.skipAll(input)
      _ <- copyValue()
      res <- parser.Whitespaces.skipAll(input)
    yield res
}
