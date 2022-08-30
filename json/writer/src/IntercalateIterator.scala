package io.github.maxkar
package json.writer


/**
 * Iterator that intercalates (i.e. "inserts") some sequences between values of
 * another iterator.
 * @param base iterator over the "base" elements (i.e. array or object elements). Could not be empty.
 * @param prefix sequence of strings to insert before the first element.
 * @param infix function used to create an iterator for elements between "base" seqeunces.
 * @param suffix elements to output after the last character.
 */
private class IntercalateIterator private(
      base: Iterator[OutputIterator],
      prefix: Iterator[CharSequence],
      infix: => Iterator[CharSequence],
      suffix: Iterator[CharSequence],
    ) extends OutputIterator:
  import IntercalateIterator._
  import OutputIterator.NextResult


  /** Iterator that we have to use for the current "basic" section. */
  private var basicIterator = prefix


  override def next(): NextResult =
    /* We are in some sequence. Put it. */
    if basicIterator.hasNext then
      return NextResult.Result(basicIterator.next())

    if !base.hasNext then return NextResult.End

    /* We both delegate to the inner component and set-up next "regular" sequence
     * that will be automatically returned.
     */
    val delegate = base.next()
    basicIterator = if base.hasNext then infix else suffix
    NextResult.Delegate(delegate)
  end next


end IntercalateIterator


private object IntercalateIterator:
  import OutputIterator.NextResult

  /** Iterator for an empty sequence of base elements. */
  private class EmptySeqIterator(
        prefix: Iterator[CharSequence],
        suffix: Iterator[CharSequence]
      ) extends OutputIterator:
    /** Indicates that we are writing prefix. */
    private var inPrefix = true


    override def next(): NextResult =
      if inPrefix then
        if prefix.hasNext then
          return NextResult.Result(prefix.next())
        else
          inPrefix = false

      if suffix.hasNext then
        NextResult.Result(suffix.next())
      else
        NextResult.End
    end next

  end EmptySeqIterator



  /**
   * Creates an iterator that intercalates (i.e. "inserts") some sequences between values of
   * another iterator.
   * @param base iterator over the "base" elements (i.e. array or object elements). Could not be empty.
   * @param prefix sequence of strings to insert before the first element.
   * @param infix function used to create an iterator for elements between "base" seqeunces.
   * @param suffix elements to output after the last character.
   */
  def apply(
        base: Iterator[OutputIterator],
        prefix: Iterator[CharSequence],
        infix: => Iterator[CharSequence],
        suffix: Iterator[CharSequence],
      ): OutputIterator =
    if base.hasNext then
      new IntercalateIterator(base, prefix, infix, suffix)
    else
      new EmptySeqIterator(prefix, suffix)

end IntercalateIterator
