package io.github.maxkar
package sql.connection

import java.sql.{Connection => JdbcConnection}


/**
 * Database connection in autocommit mode. All statements are executed and
 * commited immediately. New transactions could be started on these connections.
 */
final class AutocommitConnection(jdbcConnection: JdbcConnection)
    extends Connection(jdbcConnection):

  override def allOrNothing[T](cb: Transaction[?] => T): T =
    jdbcConnection.setAutoCommit(false)
    try
      invokeCallback(cb)
    finally
      jdbcConnection.setAutoCommit(true)
    end try
  end allOrNothing


  /** Runs the callback at the "read uncommitted" isolation level. */
  def atReadUncommitted[T](cb: Transaction[Isolation.ReadUncommitted] => T): T =
    runTx(JdbcConnection.TRANSACTION_READ_UNCOMMITTED, cb)


  /** Runs the callback at the "read committed" isolation level. */
  def atReadCommitted[T](cb: Transaction[Isolation.ReadCommitted] => T): T =
    runTx(JdbcConnection.TRANSACTION_READ_COMMITTED, cb)


  /** Runs the callback at the "repeatable read" isolation level. */
  def atRepeatableRead[T](cb: Transaction[Isolation.RepeatableRead] => T): T =
    runTx(JdbcConnection.TRANSACTION_REPEATABLE_READ, cb)


  /** Runs the callback at the "serializable" isolation level. */
  def atSerializable[T](cb: Transaction[Isolation.Serializable] => T): T =
    runTx(JdbcConnection.TRANSACTION_SERIALIZABLE, cb)


  /**
   * Runs transaction at the specific isolation level.
   * @tparam T type returned by the callback.
   * @tparam I isolation level that we would be running at (associated with level).
   * @param level JDBC transaction isolation level.
   * @param cb callback to execute on the transaction.
   */
  private def runTx[T, I](level: Int, cb: Transaction[I] => T): T =
    jdbcConnection.setAutoCommit(false)
    try
      jdbcConnection.setTransactionIsolation(level)
      invokeCallback(cb)
    finally
      jdbcConnection.setAutoCommit(true)
    end try
  end runTx


  /**
   * Invokes the callback on the transaction. Commits or rolls back the transaction
   * as needed.
   */
  private inline def invokeCallback[T, I](cb: Transaction[I] => T): T =
    val tx = new Transaction[I](jdbcConnection)
    val res =
      try
        cb(tx)
      catch
        case e: Throwable =>
          jdbcConnection.rollback()
          throw e
      end try

    if tx.rollbackOnly then
      jdbcConnection.rollback()
    else
      jdbcConnection.commit()
    end if

    res
  end invokeCallback
end AutocommitConnection
