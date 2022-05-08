package io.github.maxkar
package json.parser.chunky

import json.parser.input.ConsumerStatus

/**
 * One "operation" (or instruction) for the parser.
 * @tparam E error representation (non-local return).
 * @tparam T result type (aka "monad value").
 */
private abstract sealed class Operation[+E, +T]



object Operation:

  /** A "basic" (non-flat-map) operation. */
  private[chunky] abstract sealed class BasicOperation[+E, +T] extends Operation[E, T]


  /* MONAD OPERATIONS. */

  /** "Pure" value (or just constant) wrapped into the monad. */
  case class Pure[E, T](v: T) extends BasicOperation[E, T]

  /** Error occured during the operation, processing should be stopped. */
  case class Error[E](error: E) extends Operation[E, Nothing]

  /**
   * Flat map monadic operation.
   * @tparam E error type.
   * @tparam S source (pre-flat-map) operation.
   * @tparam T result type.
   * @param base base value (pre-monad).
   * @param fn post-map function.
   */
  case class FlatMap[E, S, T](base: BasicOperation[E, S], fn: S => Operation[E, T])
      extends Operation[E, T]


  /* INPUT OPERATIONS. */

  /** Stateful processing step (as defined by the parser input). */
  case class StatefulScan[E, S](state: S, step: (CharSequence, S) => (ConsumerStatus, S))
      extends BasicOperation[E, S]

  /** Look ahead operation - look at char and process the state with the given function. */
  case class LookAhead[E, T](fn: Char => Operation[E, T])
      extends BasicOperation[E, T]

  /** Skip one input character. */
  case object SkipChar extends BasicOperation[Nothing, Unit]

  /** Skips whitespaces from the input. */
  case object SkipWhitespaces extends BasicOperation[Nothing, Unit]


  /* MISCELANEOUS OPERATIONS. */

  /** "Get current location" operation. */
  case object Location extends BasicOperation[Nothing, SourceLocation]

end Operation
