package io.github.maxkar
package json.writer

/**
 * The main entry point to the JSON output.
 */
object Output:

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
    emptyObjectWrap: WrapEmptyOptions = wrapEmptyInsideObjects,
    emptyArrayWrap: WrapEmptyOptions = wrapEmptyInsideObjects,
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


  /**
   * Creates a json writer in a compact form. Compact form does not use any whitespaces
   * for json markup.
   */
  def compact[T <: B, B: Writeable](value: T): Writer =
    OutputIteratorAdapter(Compact(value))


  /**
   * Creates a json writer in a very pretty form according to the output options.
   */
  def pretty[T <: B, B](value: T, options: PrettyPrintOptions)(using writeable: Writeable[B]): Writer = {
    val printer = new Pretty(Vector.empty, options, Pretty.Context.TopLevel)
    OutputIteratorAdapter(writeable.decodeElement(value, printer))
  }
end Output
