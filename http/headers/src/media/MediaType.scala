package io.github.maxkar
package http.headers.media


/**
 * Media type as defined by the specification.
 * @param category MIME category (type).
 * @param subtype MIME subtype of the media.
 * @param parameters MIME parameters - map from **lowercase** names to value parameters.
 */
final case class MediaType(
      category: String,
      subtype: String,
      parameters: Map[String, String],
    ) {

  /**
   * Checks if the media type satisfies the selector.
   *
   * This is useful for pattern-matching like
   * ```
   * val UserJsonV1 = MediaType("application", "user", "f" -> "json", "v" -> "1")
   * val UserJsonV2 = MediaType("application", "user", "f" -> "json", "v" -> "2")
   *
   * val selector: MediaSelector = ???
   *
   * selector match
   *   case UserJsonV1 => ...
   *   case UserJsonV2 => ...
   * end match
   * ```
   *
   * The above match may look stupid. But this works even better in the content negotiation
   * protocols where the match forms a partial function and all the checks are carried
   * over by the negotiator.
   */
  def unapply(selector: MediaSelector): Boolean =
    selector.accepts(this)
}


object MediaType {
  /** Creates a new media type .*/
  def apply(
        category: String,
        subtype: String,
        params: (String, String)*
      ): MediaType = {
    val effectiveParams =
      params
        .map((k, v) => (k.toLowerCase(), v))
        .toMap
    new MediaType(category, subtype, effectiveParams)
  }
}
