package io.github.maxkar
package json.parser.chunky

import json.parser.input.ParserInput
import json.parser.input.ConsumerStatus

import fun.Monad


/**
 * Parsing module - the parser type and corresponding APIs.
 * @tparam E error representation (outcome of non-local "abort" functionality).
 * @param eof function used to generate "unexpected end of file" condition.
 */
class Module[E] private(eof: SourceLocation => E):

  /** Parser type. */
  type Parser[+T] = Operation[E, T]

  /** Monad typeclass implementation for this parser type. */
  given parserMonad: Monad[Parser] with
    override def pure[S](v: S): Parser[S] =
      Operation.Pure(v)

    override def bind[S, T](base: Parser[S], fn: S => Parser[T]): Parser[T] =
      base match
        case e@Operation.Error(_) =>
          /* This is abort and we do this quick. */
          e
        case Operation.FlatMap(peerBase, peerFn) =>
          /* Rewrite flat map using monadic laws. The goal is to avoid "deep calculation"
           * stacks in the evaluation module. This rewrite guarantees that the first step
           * is always a _simple_ step.
           */
          Operation.FlatMap(peerBase, bv => bind(peerFn(bv), fn))
        case other: Operation.BasicOperation[E, S] => Operation.FlatMap(other, fn)
      end match
  end parserMonad


  /** Implementation of the Parser Input - primitive "reading" operations. */
  given parserInput: ParserInput[Parser] with
    override def statefulScan[S](initialState: S, step: (CharSequence, S) => (ConsumerStatus, S)): Parser[S] =
      Operation.StatefulScan(initialState, step)

    override def lookAhead[T](selector: Char => Parser[T]): Parser[T] =
      Operation.LookAhead(selector)

    override def skipChar(): Parser[Unit] = Operation.SkipChar

    override def skipWhitespaces(): Parser[Unit] = Operation.SkipWhitespaces
  end parserInput


  /* CUSTOM OPERATIONS. */

  /** Aborts the execution with the given error. */
  def abort(error: E): Parser[Nothing] = Operation.Error[E](error)

  /** Retrieves the current location in the input stream. */
  val location: Parser[SourceLocation] = Operation.Location


  /** Starts the processing and returns the input consumer. */
  def start[T](parser: Parser[T]): InputProcessor[E, T] =
    new InputProcessor[E, T](parser, eof, parserMonad)

end Module



object Module:

  /**
   * Creates a new parsing module.
   * @tparam E error type.
   * @param unexpectedEof error factory. Invoked when end-of-file is reached
   *   but a character is expected.
   */
  def apply[E](unexpectedEof: SourceLocation => E): Module[E] =
    new Module[E](unexpectedEof)
end Module
