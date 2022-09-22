package io.github.maxkar
package json.query

/**
 * Outcome of navigating one step in the model.
 * @tparam T type of the model elements.
 */
enum ModelStepResult[+T]:
  /**
   * Navigation was successfull and the {{{value}}} is the requested child.
   * @param value value at the given selector.
   */
  case Success(value: T)

  /**
   * Requested selector was applicable to the actual value type
   * (i.e. integer index for an array) but was out of range of the supported
   * values (index out of range for an array, missing key for objects).
   */
  case MissingValue

  /**
   * The requested selector is not applicable to the given value type
   * (for example, integer index was provided for json object).
   */
  case IllegalSelector
end ModelStepResult
