package io.github.maxkar
package json.writer

/**
 * The main entry point to the JSON output.
 */
object Output:
  /**
   * Creates a json writer in a compact form. Compact form does not use any whitespaces
   * for json markup.
   */
  def compact[T <: B, B: Writeable](value: T): Writer =
    OutputIteratorAdapter(Compact(value))
end Output
