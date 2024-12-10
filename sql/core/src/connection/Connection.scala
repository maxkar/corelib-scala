package io.github.maxkar
package sql.connection

import java.sql.{Connection => JdbcConnection}
import java.sql.PreparedStatement

/**
 * An "established" connection to a database.
 *
 * The instances of this trait represents a readily available connection;
 * no additional waiting would be required to execute database queries. There
 * is not specific warranties about transaction isolation or if transaction is
 * present at all.
 *
 * @param jdbcConnection Active JDBC connection that could be used for running
 * operations. Clients using this fields could use it for running queries but
 * must not change transaction isolation level or close the connection.
 */
abstract class Connection(val jdbcConnection: JdbcConnection) {
  /** Performs a callback over the active (low-level) JDBC connection. */
  def withJdbcConnection[T](cb: JdbcConnection => T): T =
    cb(jdbcConnection)


  /**
   * Runs the callback in the "all-or-nothing" context. The callback is executed
   * inside transaction or sub-transaction (based on what this connection is) at
   * some unspecified isolation level. The only known property is atomicity of
   * the changes - either all changes would be applied or none of those would
   * be applied.
   */
  def allOrNothing[T](cb: Transaction[?] ?=> T): T
}


object Connection {
  /**
   * Executes a callback on a prepared statement obtained from this connection.
   * @param statement statement text to prepare and provide to the callback.
   * @param cb callback to execute on the prepared statement.
   * @return result of the callback.
   */
  def withPreparedStatement[T](
        statement: String
      )(
        cb: PreparedStatement => T
      )(using
        conn: Connection
     ): T = {
    val ps = conn.jdbcConnection.prepareStatement(statement)
    try
      cb(ps)
    finally
      ps.close()
  }
}
