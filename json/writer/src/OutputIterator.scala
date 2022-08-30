package io.github.maxkar
package json.writer

/**
 * Structured iterator over the JSON output nodes.
 * The iterator is more complicated than the regular one. It not only iterate
 * "regular" items but could also delegate production to another (nested)
 * output iterator and resume after that nested iterator finishes.
 */
private abstract class OutputIterator:
  /**
   * Next iterator to use when this output iterator is fully processed.
   * The field is managed by the regular "flat" iterator.
   */
  private[writer] var nextIterator: OutputIterator = null

  /**
   * Returns next element handled by this specific iterator instance or indicates
   * that there is no more input is available.
   */
  def next(): OutputIterator.NextResult

end OutputIterator


private object OutputIterator:
  /**
   * Result of processing to the next step.
   */
  enum NextResult:
    /**
     * Iteration is complete, no more outputs would be produces by the
     * output iterator. Next iterator should be used for next data portions.
     */
    case End

    /**
     * Valid output was generated by the step. The output should be returned
     * and the iterator may be queried for next actions.
     */
    case Result(value: CharSequence)


    /**
     * The iterator found that some complex structure has to be provided and
     * it is easier to delegate processing to the given "nested" iterator. After
     * handling of the nested iterator is complete, the processing may return to
     * the original iterator.
     */
    case Delegate(other: OutputIterator)
  end NextResult


  /**
   * Returns an iterator that returns a single value and then ends.
   * @param value value that will be returned by the iterator.
   */
  def single(value: CharSequence): OutputIterator =
    new OutputIterator:
      /** Indicates if an element was returned or not. */
      private var returned = false

      override def next(): NextResult =
        if returned then return NextResult.End
        returned = true
        return NextResult.Result(value)
      end next
    end new
  end single
end OutputIterator
