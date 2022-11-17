package io.github.maxkar
package http.headers.media

import javax.print.attribute.standard.Media


/**
 * Media selector (as defined by Accept header).
 * @param weight selector's weight.
 * @param category category encoding of this selector.
 * @param subtype subtype encoding of this selector.
 * @param parameters selector's parameters.
 * @param weght selector's weight.
 */
final class MediaSelector private(
      val category: String,
      val subtype: String,
      val parameters: Seq[(String, String)],
      val weight: Int,
      matcher: MediaSelector.Matcher
    ):
  /** Checks if the media type matches this selector. */
  def accepts(mt: MediaType): Boolean =
    matcher.accepts(mt)
end MediaSelector



object MediaSelector:
  private trait Matcher:
    def accepts(mt: MediaType): Boolean
  end Matcher

  /** Wildcard selector - matches everything. */
  private object WildcardMatcher extends Matcher:
    override def accepts(mt: MediaType): Boolean = true
  end WildcardMatcher


  /** Category selector - matches the given category. */
  private class CategoryMatcher(category: String) extends Matcher:
    override def accepts(mt: MediaType): Boolean =
      mt.category == category
  end CategoryMatcher


  /** Full selector - matches the given category, subtype and parameters. */
  private class FullMatcher(category: String, subtype: String, params: Seq[(String, String)]) extends Matcher:
    override def accepts(mt: MediaType): Boolean =
      if mt.category != category then return false
      if mt.subtype != subtype then return false

      val myParam = params.iterator
      while myParam.hasNext do
        val (k, v) = myParam.next()
        mt.parameters.get(k) match
          case None => return false
          case Some(value) => if value != v then return false
        end match
      end while

      return true
    end accepts
  end FullMatcher


  /** Creates a new wildcard selector. */
  def wildcard(weight: Int): MediaSelector =
    new MediaSelector("*", "*", Seq.empty, weight, WildcardMatcher)


  /** Creates a new matcher that checks that the mime type belongs to the category. */
  def category(category: String, weight: Int): MediaSelector =
    new MediaSelector(category, "*", Seq.empty, weight, new CategoryMatcher(category))


  /** Creates a new full matcher that checks category, subtype and parameters. */
  def full(category: String, subtype: String, parameters: Seq[(String, String)], weight: Int): MediaSelector =
    val effectiveParams = parameters.map((k, v) => (k.toLowerCase(), v))
    new MediaSelector(category, subtype, parameters, weight, new FullMatcher(category, subtype, effectiveParams))
  end full


  /**
   * Ordering on the selector objects. Selectors with higher weight come earlier
   * than selectors with lower weight.
   */
  given selectorOrdering: Ordering[MediaSelector] with
    override def compare(a: MediaSelector, b: MediaSelector): Int =
      if a.weight > b.weight then -1
      else if a.weight < b.weight then 1
      else 0
    end compare
  end selectorOrdering
end MediaSelector
