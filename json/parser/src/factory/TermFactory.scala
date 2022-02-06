package io.github.maxkar
package json.parser.factory

/**
 * Factory for one of the JSON term (literal) values - true, false or null.
 * @tparam M operation type.
 * @tparam J json value representation.
 */
trait TermFactory[M[_], J]:
  /** Parsing state. */
  type State

  /** Starts parsing of the term. */
  def begin: M[State]

  /**
   * Ends parsing of the term and returns its json representation.
   * @param state state captured at the start of the term.
   */
  def end(state: State): M[J]

  /**
   * Handles an "invalid character" situation.
   * @param state state captured at the start of the term.
   * @param expectedTerm the representation expected in the input.
   * @param offset offset of the first invalid character. The input points
   *   to this invalid character, previous (matching) characters are consumed.
   */
  def badInput(state: State, expectedTerm: String, offset: Int): M[J]


  /**
   * Handles an "incomplete input" situation.
   * @param state state captured at the start of the term.
   * @param expectedTerm the representation expected in the input.
   * @param offset position where the input ended (first missing character).
   */
  def unexpectedEnd(state: State, expectedTerm: String, offset: Int): M[J]

end TermFactory
