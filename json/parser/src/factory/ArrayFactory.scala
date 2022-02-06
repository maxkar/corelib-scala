package io.github.maxkar
package json.parser.factory

/**
 * Factory for Json Array values.
 * @tparam M operation type.
 * @tparam J json value representation.
 */
trait ArrayFactory[M[_], J]:
  /** Parsing state. */
  type State

  /** Starts parsing of an array. */
  def begin: M[State]

  /**
   * Ends parsing of the array and returns its json representation.
   * @param state state updated through the process.
   */
  def end(state: State): M[J]

  /**
   * Updates the current array state with an additional element.
   * The method is first called on the state returned from the `begin` call. All
   * consequent calls receive input from the previous call to one of the `update` methods.
   *
   * @param state array state so far.
   * @param input next value of the array.
   * @return pair of (<new state>, <should continue>). If the <should continue> is
   *   set to false, the parser will abort array processing (regardless of its) internal
   *   state and will invoke the `end` method.
   */
  def update(state: State, input: J): (State, Boolean)

  /**
   * Handles a bad array continuation - the state where comma or closing bracket was expected
   * but something else occured in the input.
   * @param state array state before the issue has happened.
   */
  def badArrayContinuation(state: State): M[J]

end ArrayFactory
