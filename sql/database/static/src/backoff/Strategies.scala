package io.github.maxkar
package sql.database.static.backoff


/** Some simple back-off strategies. */
object Strategies:
  /** Fixed back-off strategy. */
  def fixed(timeoutMs: Int): BackoffFactory =
    new SimpleBackoffFactory(timeoutMs, timeoutMs, identity)


  /**
   * Randomized exponential back-off factory. Uses exponent of 2 and
   * spread factor equal to the "current" timeout. In simple terms,
   * for the next timeout it generates value between `2*currentTimeout` and
   * `3*currentTimeout`.
   *
   * @param minValueMs minimal timeout value.
   * @param maxValueMs maximal timeout value.
   */
  def randomizedDoubleExponent(minValueMs: Int, maxValueMs: Int): BackoffFactory =
    new SimpleBackoffFactory(minValueMs, maxValueMs, v => (v << 2) + scala.util.Random.nextInt(v))
end Strategies
