package io.github.maxkar
package json.parser

/**
 * A flexible abstraction over an input stream for the parser.
 *
 * This trait is more sophisticated than the `BasicInput` and is aware
 * of some JSON-specific components and features. These extension points
 * could be used to provide faster (or more optimal in other way) implementation.
 *
 * @tparam M type of the operation/processing.
 */
trait ParserInput[M[_]]:
  /**
   * Statefully processes the input until no more could be consumed by the
   * `step` function or until end of input is reached.
   *
   * The contract of this function is the same as [[BasicInput.statefulScan]]
   *
   * @tparam S type of the consumer's state.
   * @param initialState initial operation state.
   * @param step function to advance the `current` state based on the input data.
   *   It returns the output (if more input is expected in the returned state) and
   *   the new state (which should be passed as an input to the next operation).
   * @return resulting state at either the end of the input or at the moment when
   *   the `step` function returned ConsumerStatus.Finished (whatever happens first).
   * @see [[BasicInput.statefulScan]]
   */
  def statefulScan[S](initialState: S, step: (CharSequence, S) => (ConsumerStatus, S)): M[S]


  /**
   * Skips whitespace characters from the input stream. Four characters are treated
   * as standard JSON whitespaces: ' ' (space), '\r' (carriage return), '\n' (linefeed)
   * and '\t' (tabulation).
   *
   * This is the only place where newlines and linefeeds could be processed by the
   * standard parser. Note that error recovery mechanisms in specific JSON factories may
   * also skip over new lines, the contract between character input and json factories is outside
   * of this specification.
   *
   * The implementation _may_ also skip over comments (non-standard JSON feature).
   *
   * The input stream should "point" to a non-whitespace character after the operation is complete.
   */
  def skipWhitespaces(): M[Unit]


  /**
   * Selects a next parser based on the look-ahead character.
   *
   * The goal of this method is to delegate parsing to a specific parser (for example, to the Number parser)
   * without consuming any input. The specialized parser needs to have access to the input. It also needs
   * to preserve exact input position for JSON factories which may want to capture exact value location.
   *
   * The selector (choice) _does not_ consume input (i.e. does not advance position in the input stream).
   *
   * @param selector a function to choose the next parser based on one look-ahead character.
   * @return result of applying the parser returned by the `select` function.
   */
  def lookAhead[T](selector: Char => M[T]): M[T]

end ParserInput
