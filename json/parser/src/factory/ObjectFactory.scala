package io.github.maxkar
package json.parser.factory

/**
 * Factory for Json Object values.
 * @tparam M operation type.
 * @tparam J json value representation.
 */
trait ObjectFactory[M[_], J]:
  /** Parsing state. */
  type State

  /** Type of the object key. */
  type Key

  /** Factory for object keys. */
  val keyFactory: StringFactory[M, Key]

  /** Starts parsing of an object. */
  def begin: M[State]

  /**
   * Ends parsing of the object and returns its json representation.
   * @param state state updated through the process.
   */
  def end(state: State): M[J]

  /**
   * Updates the current object state with an additional key-value pair.
   * The method is first called on the state returned from the `begin` call. All
   * consequent calls receive input from the previous call to one of the `update` methods.
   *
   * @param state object state so far.
   * @param key key definition.
   * @param value value definition.
   * @return pair of (<new state>, <should continue>). If the <should continue> is
   *   set to false, the parser will abort object processing (regardless of its) internal
   *   state and will invoke the `end` method.
   */
  def update(state: State, key: Key, value: J): (State, Boolean)

  /** Handles a situation where json key start is not valid. */
  def badKeyStart(state: State): M[J]

  /**
   * Handles a bad object continuation - the state where comma or closing bracket was expected
   * but something else occured in the input.
   * @param state array state before the issue has happened.
   */
  def badObjectContinuation(state: State): M[J]

  /** Handles a situation where bad key-value separator occured in the input. */
  def badKeyValueSeparator(state: State, key: Key): M[J]

end ObjectFactory
