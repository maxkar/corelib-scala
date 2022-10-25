package io.github.maxkar
package json.writer

/**
 * Pretty print configuration.
 *
 * @param indent indent to use for nested object.
 * @param sortObjectKeys if set to true, objects keys will be sorted. Otherwise the
 *   entries would be output in the internal order. Setting this to true may involve
 *   performance penalty.
 * @param emptyObjectWrap how/when to wrap empty objects.
 * @param emptyArrayWrap how/when wrap empty arrays.
 */
case class PrettyPrintOptions(
  indent: String = "  ",
  sortObjectKeys: Boolean = false,
  emptyObjectWrap: PrettyPrintOptions.WrapEmptyOptions = wrapEmptyInsideObjects,
  emptyArrayWrap: PrettyPrintOptions.WrapEmptyOptions = wrapEmptyInsideObjects,
)


/**
 * Options for pretty-printing JSON values.
 */
object PrettyPrintOptions:
  /**
   * Wrap options for objects and arrays.
   * @param wrapInObjects set to true when values should use newline when occurs inside object.
   * I.e (when enabled)
   * {{{
   * "field": {
   * },
   * "field2": [
   * ]
   * }}}
   * @param wrapInArrays set to true when values should wrap to new line inside arrays.
   * When enabled, formats objects like:
   * {{{
   * [
   *   {
   *   },
   *   [
   *   ]
   * ]
   * }}}
   *
   * @param wrapAtTopLevel set to true when values should wrap at the top level. When
   * enabled, top-level objects looks like
   * {{{
   * {
   * }
   * }}}
   *
   */
  case class WrapEmptyOptions(
    wrapInObjects: Boolean,
    wrapInArrays: Boolean,
    wrapAtTopLevel: Boolean,
  )

  /** Wrap non-empty objects only, don't wrap empty objects anywere. */
  val noWrapEmpty = WrapEmptyOptions(wrapInObjects = false, wrapInArrays = false, wrapAtTopLevel = false)

  /**
   * Wraps empty values when they occur inside objects but don't wrap them
   * inside arrays (because it looks a bit ugly) or when empty object is at the top level.
   */
  val wrapEmptyInsideObjects = WrapEmptyOptions(wrapInObjects = true, wrapInArrays = false, wrapAtTopLevel = false)

  /**
   * Always wrap empty objects.
   */
  val alwaysWrapEmpty = WrapEmptyOptions(wrapInObjects = true, wrapInArrays = true, wrapAtTopLevel = true)
end PrettyPrintOptions
