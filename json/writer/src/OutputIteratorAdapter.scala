package io.github.maxkar
package json.writer

import io.github.maxkar.json.writer.OutputIterator.NextResult


/**
 * Adapter from output iterator to a regular iterator.
 * @param currentIterator iterator to adapt.
 */
private class OutputIteratorAdapter(
      private var currentIterator: OutputIterator)
    extends Iterator[CharSequence]:

  /** Next result to return. */
  private var nextResult: CharSequence = advance()


  override def hasNext: Boolean = nextResult != null

  override def next(): CharSequence = nextResult


  /**
   * Advances over the iterator(s) and returns the next element
   * that has to be returned.
   */
  private def advance(): CharSequence =
    while currentIterator != null do
      currentIterator.next() match
        case NextResult.End =>
          currentIterator = currentIterator.nextIterator
        case NextResult.Result(v) =>
          return v
        case NextResult.Delegate(delegate) =>
          delegate.nextIterator = currentIterator
          currentIterator = delegate
      end match
    end while

    null
  end advance
end OutputIteratorAdapter
