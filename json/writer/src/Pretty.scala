package io.github.maxkar
package json.writer

/**
 * Pretty printer.
 *
 * @param indent indent of the current print.
 * @param options options for pretty printing.
 * @param context context of the operation - where we are.
 */
private final class Pretty[B](
      indent: Vector[String],
      options: Output.PrettyPrintOptions,
      context: Pretty.Context,
    )(
      using writeable: Writeable[B]
    )
    extends ValueVisitor[B, OutputIterator]:

  override def boolean(v: Boolean): OutputIterator =
    Primitives.bool(v)

  override def number(representation: CharSequence): OutputIterator =
    Primitives.number(representation)

  override def string(v: CharSequence): OutputIterator =
    Primitives.string(v)

  override def nullValue(): OutputIterator =
    Primitives.nullValue()

  override def array(iter: Iterator[B]): OutputIterator =
    if iter.isEmpty then
      if context.shouldWrap(options.emptyArrayWrap) then
        OutputIterator.sequence("[\n" +: indent :+ "]")
      else
        OutputIterator.single("[]")
    else
      val newIndent = options.indent +: indent
      val nestedPrinter = new Pretty(newIndent, options, Pretty.Context.InArray)
      return IntercalateIterator(
        base = iter.map(writeable.decodeElement(_, nestedPrinter)),
        prefix = ("[\n" +: newIndent).iterator,
        infix = (",\n" +: newIndent).iterator,
        suffix = ("\n" +: indent :+ "]").iterator
      )
  end array

  override def unorderedObject(iter: Iterator[(String, B)]): OutputIterator =
    if options.sortObjectKeys then
      val tmp = iter.toSeq.sortBy((_._1))
      objectAsProvided(tmp.iterator)
    else
      objectAsProvided(iter)


  override def orderedObject(iter: Iterator[(String, B)]): OutputIterator =
    objectAsProvided(iter)


  /**
   * Prints the object "as provided" by the given iterator.
   */
  private def objectAsProvided(iter: Iterator[(String, B)]): OutputIterator =
    if iter.isEmpty then
      if context.shouldWrap(options.emptyObjectWrap) then
        OutputIterator.sequence("{\n" +: indent :+ "}")
      else
        OutputIterator.single("{}")
    else
      val newIndent = options.indent +: indent
      val nestedPrinter = new Pretty(newIndent, options, Pretty.Context.InObject)
      return IntercalateIterator(
        base = iter.map((k, v) => ObjectEntry(k, ": ", writeable.decodeElement(v, nestedPrinter))),
        prefix = ("{\n" +: newIndent).iterator,
        infix = (",\n" +: newIndent).iterator,
        suffix = ("\n" +: indent :+ "}").iterator
      )
  end objectAsProvided
end Pretty


private object Pretty:
  /**
   * Where we are in the process of printing.
   */
  enum Context:
    /** We are at the root. */
    case TopLevel
    /** We are inside object (field). */
    case InObject
    /** We are inside array. */
    case InArray


    /** Checks if things should be wrapped based on options and context. */
    def shouldWrap(options: Output.WrapEmptyOptions): Boolean =
      this match
        case TopLevel => options.wrapAtTopLevel
        case InObject => options.wrapInObjects
        case InArray => options.wrapInArrays
      end match
    end shouldWrap
  end Context
end Pretty
