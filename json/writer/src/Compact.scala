package io.github.maxkar
package json.writer

/**
 * Compact json writer.
 */
private object Compact:
  /** Creates a compact output iterator for the given writeable value. */
  def apply[T <: B, B](value: T)(using writeable: Writeable[B]): OutputIterator =
    object visitor extends ValueVisitor[B, OutputIterator]:

      override def boolean(v: Boolean): OutputIterator =
        Primitives.bool(v)

      override def number(representation: CharSequence): OutputIterator =
        Primitives.number(representation)

      override def string(v: CharSequence): OutputIterator =
        Primitives.string(v)

      override def nullValue(): OutputIterator =
        Primitives.nullValue()

      override def array(iter: Iterator[B]): OutputIterator =
        IntercalateIterator(
          base = iter.map(writeable.decodeElement(_, this)),
          prefix = Iterator.single("["),
          infix = Iterator.single(","),
          suffix = Iterator.single("]")
        )

      override def unorderedObject(iter: Iterator[(String, B)]): OutputIterator =
        IntercalateIterator(
          base = iter.map((k, v) => ObjectEntry(k, ":", writeable.decodeElement(v, this))),
          prefix = Iterator.single("{"),
          suffix = Iterator.single("}"),
          infix = Iterator.single(",")
        )
    end visitor

    writeable.decodeElement(value, visitor)

end Compact
