package io.github.maxkar
package sql.connection


/**
 * Collection of types denoting standard (JDBC) transaction isolation
 * and relationships between them.
 */
object Isolation {
  /** The minimal isolation - read uncommitted. */
  abstract sealed class ReadUncommitted

  /** Read committed isolation level. */
  abstract sealed class ReadCommitted extends ReadUncommitted

  /** Repeatable read isolation. */
  abstract sealed class RepeatableRead extends ReadCommitted

  /** Serializable isolation level - the maximum standard isolation level. */
  abstract sealed class Serializable extends RepeatableRead
}
