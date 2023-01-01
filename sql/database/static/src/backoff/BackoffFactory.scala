package io.github.maxkar
package sql.database.static.backoff


/**
 * Factory for back-off strategies. Individual back-off instances
 * are responsible for managing backoff timeouts for one object only
 * (connection, thread, etc...). In many cases multiple managed objects
 * exists and the factory is used to create individual back-off instances
 * (similar to threadFactory).
 */
trait BackoffFactory:
  /** Creates a new backoff instance. */
  def newBackoff(): Backoff
end BackoffFactory
