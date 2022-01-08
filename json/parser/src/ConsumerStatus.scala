package io.github.maxkar
package json.parser

/**
 * Status of processing the input sequence. Indicates how much input was
 * consumed and if more input is needed.
 */
enum ConsumerStatus:
  /** All provided input was consumed and the next portion may be fed to the step function. */
  case NeedMoreInput

  /**
   * The consumer is "fed-up" and does not want to process more input.
   * @param consumed the amount of characters that was "consumed" by the parser.
   */
  case Finished(consumed: Int)

end ConsumerStatus
