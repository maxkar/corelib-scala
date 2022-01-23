package io.github.maxkar
package json.parser.factory

/**
 * Factory for one of the JSON string values.
 * @tparam M operation type.
 * @tparam J json value representation.
 */
trait StringFactory[M[_], J]:
  /** Parsing state. */
  type State

  /** Starts parsing of a string. */
  def begin: M[State]

  /**
   * Updates the current string representation with some extra characters.
   * The method is first called on the state returned from the `begin` call. All
   * consequent calls receive input from the previous call to one of the `update` methods.
   *
   * This method is used for passing batch of "regular" string characters to the factory.

   * @param state string state so far.
   * @param input next portion of characters.
   * @return pair of (<new state>, <should continue>). If the <should continue> is
   *   set to false, the parser will abort string processing (regardless of its) internal
   *   state and will invoke the `end` method. If <should continue> is true, the parser
   *   will act according to the input characters - it may invoke one of the `update` methods again passing
   *   more characters belonging to the string, it may invoke `end` when the full string
   *   was consumed from the input or it may invoke any of the `error` methods to indicate
   *   malformed input.
   */
  def update(state: State, input: CharSequence): (State, Boolean)

  /**
   * Updates the current string representation with some extra characters.
   * The method is first called on the state returned from the `begin` call. All
   * consequent calls receive input from the previous call to one of the `update` methods.
   *
   * This method is used with (decoded) escape sequences.
   *
   * @param state string state so far.
   * @param input next input character.
   * @param inputWidth width occupied by the character in the input (2 for simple escapes and 5 for unicode escapes).
   * @return pair of (<new state>, <should continue>). If the <should continue> is
   *   set to false, the parser will abort srnting processing (regardless of its) internal
   *   state and will invoke the `end` method. If <should continue> is true, the parser
   *   will act according to the input characters - it may invoke one of the `update` methods again passing
   *   more characters belonging to the string, it may invoke `end` when the full string
   *   was consumed from the input or it may invoke any of the `error` methods to indicate
   *   malformed input.
   */
  def update(state: State, input: Char): (State, Boolean)

  /**
   * Ends parsing of the string and returns its json representation.
   * @param state state updated throughout the string.
   */
  def end(state: State): M[J]

  /**
   * Handles a situation where an escape sequence is not supported by the JSON standard. The '\' character
   * is consumed but the escape itself is not.
   */
  def badEscape(state: State): M[J]

  /**
   * Handles a situation where an unicode escape should be in the input but the unicode
   * point is not correctly escaped (for example, "\uX").
   * @param state state before the unicode character was parsed.
   */
  def badUnicodeEscape(state: State): M[J]

  /**
   * Handles a situation where an unicode escape should be in the input but the unicode
   * point is not correctly escaped (for example, "\uAX").
   * @param state state before the unicode character was parsed.
   * @param c1 first valid unicode-escape character.
   */
  def badUnicodeEscape(state: State, c1: Char): M[J]

  /**
   * Handles a situation where an unicode escape should be in the input but the unicode
   * point is not correctly escaped (for example, "\uABX").
   * @param state state before the unicode character was parsed.
   * @param c1 first valid unicode-escape character.
   * @param c2 second valid unicode-escape character.
   */
  def badUnicodeEscape(state: State, c1: Char, c2: Char): M[J]

  /**
   * Handles a situation where an unicode escape should be in the input but the unicode
   * point is not correctly escaped (for example, "\uABX").
   * @param state state before the unicode character was parsed.
   * @param c1 first valid unicode-escape character.
   * @param c2 second valid unicode-escape character.
   * @param c3 third valid unicode-escape character.
   */
  def badUnicodeEscape(state: State, c1: Char, c2: Char, c3: Char): M[J]

  /**
   * Bad character occured in the string ('\r', '\n', anything with code less than 0x20).
   */
  def badCharacter(state: State): M[J]

  /** Handles a situation where input has ended but no string terminator character occured. */
  def unterminatedString(state: State): M[J]
end StringFactory
