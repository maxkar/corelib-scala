package io.github.maxkar
package sql.connection

import java.sql.{Connection => JdbcConnection}

/**
 * Representation of an active database transaction at the
 * `I` isolation level. Instances of this class provide access to the
 * underlying jdbc connection but also serve as an evidence of the
 * appropriate configuration.
 *
 * The isolation level parameter could be used by methods joining the
 * transactions at the given level. The method may either require specific
 * isolation in its arguments (the exact isolation) or use generic boundaries
 * to accept a range of the isolation levels. For example:
 *
 * ```
 *   // only work with repeatable read
 *   def doWithRepeatableRead(tx: Transaction[Isolation.RepeatableRead]) = ???
 *
 *   // At least read committed is required
 *   def atLeastRepeatableRead[T <: Isolation.RepeatableRead](tx: Transaction[T]) = ???
 * ```
 *
 * @tparam I isolation level of the current transaction.
 */
final class Transaction[I](jdbcConnection: JdbcConnection) extends Connection(jdbcConnection):
  /** Indicates that the transaction was marked as rollback-only. */
  private[connection] var rollbackOnly = false


  /**
   * Marks the transaction as rollback-only. Further queries may read and modify data but
   * all the changes will eventually be rolled back to the state before the transaction.
   */
  def setRollbackOnly(): Unit =
    rollbackOnly = true


  /**
   * Performs a "nested transaction" that may be rolled back without rolling back
   * this (parent) transaction.
   *
   * JDBC does not support nested transactions so this functionality is usually implemented
   * using savepoints.
   *
   * @param callback callback to execute on the "nested" transaction.
   */
  def nested[T](callback: Transaction[I] ?=> T): T =
    val savepoint = jdbcConnection.setSavepoint()
    val nestedTx = new Transaction[I](jdbcConnection)

    /* We calculate value in its own try/catch and handle
     * rollback separately. We need to do this to differentiate
     * failures caused by the execution (casuses rollback) or
     * caused by a failed rollback (usually due to setRollbackOnly
     * on the child, it is incorrect to attempt another rollback here).
     */
    val res =
      try
        callback(using nestedTx)
      catch
        case e: Throwable =>
          jdbcConnection.rollback(savepoint)
          throw e
      end try

    if nestedTx.rollbackOnly then
      jdbcConnection.rollback(savepoint)
    return res
  end nested


  override def allOrNothing[T](cb: Transaction[?] ?=> T): T =
    nested(cb)
end Transaction
