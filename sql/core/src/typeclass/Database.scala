package io.github.maxkar
package sql.typeclass

import sql.connection.AutocommitConnection
import sql.connection.Transaction
import sql.connection.Isolation

/**
 * A general database access API - ability to get connections and
 * execute some code on them.
 *
 * All database access code is non-generic synchronous code. This
 * reinforces constraints imposed by the JDBC API - connections are
 * blocking (synchronous). The database code should be fast and focused
 * on the database operations only, it should not perform other types
 * of IO like file reading or network communication calls.
 */
trait Database[M[_]]:
  /**
   * Executes callback on the autocommit connection. The database may
   * delay execution of the block until an actual database connection
   * is available.
   */
  def withAutocommitConnection[T](cb: AutocommitConnection ?=> T): M[T]
end Database


object Database:
  /** Executes the code block with the default autocommit mode. */
  def withAutocommit[M[_], T](
        cb: AutocommitConnection ?=> T
      )(implicit
        db: Database[M]
      ): M[T] =
    db.withAutocommitConnection(cb)


  /** Executes the code block within the "read uncommitted" transaction. */
  def atReadUncommitted[M[_], T](
        cb: Transaction[Isolation.ReadUncommitted] ?=> T
      )(implicit
        db: Database[M]
      ): M[T] =
    db.withAutocommitConnection(conn ?=> conn.atReadUncommitted(cb))


  /** Executes the code block within the "read committed" transaction. */
  def atReadCommitted[M[_], T](
        cb: Transaction[Isolation.ReadCommitted] ?=> T
      )(implicit
        db: Database[M]
      ): M[T] =
    db.withAutocommitConnection(conn ?=> conn.atReadCommitted(cb))


  /** Executes the code block within the "repeatable read" transaction. */
  def atRepeatableRead[M[_], T](
        cb: Transaction[Isolation.RepeatableRead] ?=> T
      )(implicit
        db: Database[M]
      ): M[T] =
    db.withAutocommitConnection(conn ?=> conn.atRepeatableRead(cb))


  /** Executes the code block within the "serializable" transaction. */
  def atSerializable[M[_], T](
        cb: Transaction[Isolation.Serializable] ?=> T
      )(implicit
        db: Database[M]
      ): M[T] =
    db.withAutocommitConnection(conn ?=> conn.atSerializable(cb))

end Database
