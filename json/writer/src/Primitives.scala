package io.github.maxkar
package json.writer


/**
 * Writers and formatters for primitive values.
 */
private object Primitives:
  /** Creates a writer for the boolean True value. */
  def bool(value: Boolean): Writer = Iterator.single(if value then "true" else "false")


  /** Creates a writer for a simple number (based on the number representation). */
  def number(repr: CharSequence): Writer = Iterator.single(repr)


  /** Creates a writer for a string. */
  def string(value: CharSequence): Writer = new StringWriter(value)
end Primitives
