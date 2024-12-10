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
final class Transaction[I](jdbcConnection: JdbcConnection) extends Connection(jdbcConnection) {
  /** Indicates that the transaction was marked as rollback-only. */
  private[connection] var rollbackOnly = false


  /** Listeners for the commit event. */
  private[connection] var commitListeners: Vector[() => Unit] = Vector.empty


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
  def nested[T](callback: Transaction[I] ?=> T): T = {
    val savepoint = jdbcConnection.setSavepoint()
    val nestedTx = new Transaction[I](jdbcConnection)

    /* We calculate value in its own try/catch and handle
     * rollback separately. We need to do this to differentiate
     * failures caused by the execution (casuses rollback) or
     * caused by a failed rollback (usually due to setRollbackOnly
     * on the child, it is incorrect to attempt another rollback here).
     */
    val res =
      try {
        callback(using nestedTx)
      } catch {
        case e: Throwable =>
          jdbcConnection.rollback(savepoint)
          throw e
      }

    if nestedTx.rollbackOnly then
      jdbcConnection.rollback(savepoint)
    else
      commitListeners ++= nestedTx.commitListeners
    return res
  }



  /**
   * Adds a function to invoke when the transaction is committed in the database.
   *
   * If this method is called on the nested transaction, then the callback will be called
   * only if (and when) the topmost transaction is committed, not when the nested transaction
   * completes successfully. In other words, callbacks are invoked only when all
   * the data is successfully stored in the database, not when the "logical" transaction
   * is complete.
   *
   * This method is useful for two-phase (or multi-phase) processing where certains steps
   * are dependent on the data being persisted (and transaction completing successfully).
   * For example, a notification service may persist a pending notification message during
   * the transaction. The message is forwarded to the message queue after the transaction
   * completes and should not be sent if transaction is rolled back for any reason.
   *
   * @param callback callback to invoke.
   */
  def onCommit(callback: () => Unit): Unit =
    commitListeners :+= callback


  override def allOrNothing[T](cb: Transaction[?] ?=> T): T =
    nested(cb)
}
