package io.github.maxkar
package json.writer


/**
 * Writers and formatters for primitive values.
 */
private object Primitives:
  /** Creates a writer for the boolean True value. */
  def bool(value: Boolean): OutputIterator =
    OutputIterator.single(if value then "true" else "false")


  /** Creates a writer for a simple number (based on the number representation). */
  def number(repr: CharSequence): OutputIterator =
    OutputIterator.single(repr)


  /** Creates a writer for null literal. */
  def nullValue(): OutputIterator =
    OutputIterator.single("null")


  /** Creates a writer for a string. */
  def string(value: CharSequence): OutputIterator = new StringOutputIterator(value)
end Primitives
