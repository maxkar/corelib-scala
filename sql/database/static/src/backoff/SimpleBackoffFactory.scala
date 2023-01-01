package io.github.maxkar
package sql.database.static.backoff


/**
 * Factory for back-offs that could be described using a simple timeout derivation strategy.
 *
 * @param initialBackoffMs initial value of the back-off. Used on a first
 *   attempt to set the back-off.
 * @param  maxBackoffMs maximal back-off timeout, timeouts after this value
 *   would be clipped to it.
 * @param nextTimeout function used to derive next timeout from the current one.
 */
final class SimpleBackoffFactory(
      initialBackoffMs: Int,
      maxBackoffMs: Int,
      nextTimeout: Int => Int
    ) extends BackoffFactory:

  override def newBackoff(): Backoff =
    return new SimpleBackoff(initialBackoffMs, maxBackoffMs, nextTimeout)
end SimpleBackoffFactory
