package io.github.maxkar
package sql.query


/**
 * A definition of the query timeout. Used as a given instance in the
 * most popular API(s).
 *
 * @param timeoutSeconds query timeout in seconds.
 */
final class Timeout(val timeoutSeconds: Int)

object Timeout {
  /** Default query timeout value. */
  val defaultTimeout = new Timeout(15)


  /** Creates a timeout with the given seconds value. */
  def fromSeconds(timeoutSec: Int): Timeout =
    new Timeout(timeoutSec)


  /**
   * Creates a timeout with the given milliseconds value. This
   * method rounds up the timeout to seconds as this is the miminal
   * allowed JDBC resolution. This method is provided for configuration
   * convenience and consistency for systems using milliseconds
   * throughout for its configuration.
   */
  def fromMillis(millis: Int): Timeout =
    fromSeconds((millis + 999) / 1000)
}
