package io.github.maxkar
package json.writer

/**
 * Writer for an object entry.
 * @param name name of the object.
 * @param separator name/value separator.
 * @param value writer for the value.
 */
private class ObjectEntry(
      name: CharSequence,
      separator: CharSequence,
      value: OutputIterator
    ) extends OutputIterator:
  import OutputIterator.NextResult

  /** Current parsing stage. */
  private var stage = 0


  override def next(): NextResult =
    stage match
      case 0 =>
        stage = 1
        NextResult.Delegate(Primitives.string(name))
      case 1 =>
        stage = 2
        NextResult.Result(separator)
      case 2 =>
        stage = 3
        NextResult.Delegate(value)
      case _ =>
        NextResult.End
    end match
  end next
end ObjectEntry
