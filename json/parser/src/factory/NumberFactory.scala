package io.github.maxkar
package json.parser.factory


/**
 * Factory for Json Number values.
 * @tparam M operation type.
 * @tparam J json value representation.
 */
trait NumberFactory[M[_], J]:
  /** Parsing state. */
  type State

  /** Starts parsing of a number. */
  def begin: M[State]

  /**
   * Updates the current number representation with some extra characters.
   * The method is first called on the state returned from the `begin` call. All
   * consequent calls receive input from the previous call to the `update` method.
   * @param state number state so far.
   * @param input next portion of digits and other numeric characters.
   * @return pair of (<new state>, <should continue>). If the <should continue> is
   *   set to false, the parser will abort number processing (regardless of its) internal
   *   state and will invoke the `end` method. If <should continue> is true, the parser
   *   will act according to the input characters - it may invoke the `update` again passing
   *   more characters belonging to the number, it may invoke `end` when the full number
   *   was consumed from the input or it may invoke any of the `error` methods to indicate
   *   malformed input.
   */
  def update(state: State, input: CharSequence): (State, Boolean)

  /**
   * Ends parsing of the number and returns its json representation.
   * @param state state retruned from the last `update` operation.
   */
  def end(state: State): M[J]

  /** Handles a situation with the missing integer part of the number. */
  def missingIntDigits(state: State): M[J]

  /** Handles a situation with the missing fractional part of the number (after the decimal dot). */
  def missingFractionalDigits(state: State): M[J]

  /** Handles a situation with the missing exponent digits (after exponent indicator). */
  def missingExponentDigits(state: State): M[J]

  /** Handles a situation where decimal digits occured after the leading 0 (in the integer part of the number). */
  def digitsAfterLeadingZero(state: State): M[J]
end NumberFactory
