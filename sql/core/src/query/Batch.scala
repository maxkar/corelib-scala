package io.github.maxkar
package sql.query

import java.sql.PreparedStatement

import sql.connection.Connection
import java.sql.SQLException
import java.sql.BatchUpdateException

/**
 * Batch processing utility able to use JDBC SQL Batch capabilities and re-use
 * the same prepared statement for multiple operations.
 */
object Batch {
  /** Default size of the batch, used when no custom size was specified. */
  val defaultBatchSize = 1000


  /**
   * Executes the update queries in a "batch" manner where possible.
   * The queries adds to the batch if (and only if) they produce exactly the same
   * query text (but they may have varying parameters). Otherwise multiple JDBC
   * batches may be issued.
   *
   * Usage:
   * ```
   * Batch {
   *   for datum <- data yield
   *     sql"""..."""
   * }
   * ```
   *
   * @return update counts for each query.
   */
  def apply(queries: Iterable[Query])(using Connection): Seq[Int] =
    execute(defaultBatchSize, queries)


  /**
   * Similar to apply but with the custom configuration:

   * Usage:
   * ```
   * Batch.configure(batchSize = 10) {
   *   for datum <- data yield
   *     sql"""..."""
   * }
   * ```
   */
  def configure(
        batchSize: Int = defaultBatchSize
      )(
        queries: Iterable[Query]
      )(using
        Connection
      ): Seq[Int] =
    execute(batchSize, queries)


  /** Executes the batch with the given size and queries. */
  private def execute(batchSize: Int, queries: Iterable[Query])(using conn: Connection): Seq[Int] = {
    val itr = queries.iterator
    if !itr.hasNext then
      return Seq.empty

    val ctx = new Context(batchSize, conn)
    try {
      while itr.hasNext do
        ctx.next(itr.next())

      return ctx.finish()
    } catch {
      case e: SQLException =>
        ctx.raise(e)
      case e: Throwable =>
        ctx.abort()
        throw e
    }
  }


  /** Context of one operation. */
  private class Context(maxBatchSize: Int, conn: Connection) {
    import scala.collection.mutable.ArrayBuffer

    /** Offset of the current batch first element in the overall list. */
    private var batchOffset = 0

    /**
     * Number of entries in the "current" batch that was collected but
     * was not (yet) sent to the database.
     */
    private var thisBatchSize = 0

    /** Last query text used for creating a statement. */
    private var lastText: String = null

    /** Last-used prepared statement. */
    private var ps: PreparedStatement = null

    /** Update counts for the statements. */
    private var updateCounts = new ArrayBuffer[Int]


    /** Sends next query to the database. */
    def next(query: Query): Unit = {
      val text = query.getQueryText()
      if text != lastText then
        setNewQuery(text)
      else if thisBatchSize == maxBatchSize then
        flush()

      query.setParameters(ps, 1)
      ps.addBatch()
      thisBatchSize += 1
    }


    /** Finishes the processing. */
    def finish(): Seq[Int] = {
      flush()
      closeStatement()
      return updateCounts.toSeq
    }


    /** Raises a batch update exception. */
    def raise(e: SQLException): Nothing = {
      abort()
      throw new BatchException(
          updateCounts = updateCounts.toSeq,
          batchOffset = batchOffset,
          batchSize = thisBatchSize,
          statementText = lastText,
          cause = e
      )
    }


    /** Aborts the execution and cleans up resources. */
    def abort(): Unit = {
      try {
        closeStatement()
      } catch {
        case _: Throwable =>
          /* Ignore issues in close as we'll be throwing another exception. */
          ()
      }
    }


    /** Sets the new query for the database. */
    private def setNewQuery(newQuery: String): Unit = {
      /* Send the existing portion if needed. */
      flush()

      /* Close the existing prepared statement - these should be closed. */
      closeStatement()

      /* Init new statement. */
      lastText = newQuery
      ps = conn.jdbcConnection.prepareStatement(newQuery)
    }


    /** Flushes the current batch. */
    private def flush(): Unit = {
      if thisBatchSize == 0 then
        return

      updateCounts ++= ps.executeBatch()
      batchOffset += thisBatchSize
      thisBatchSize = 0
    }


    /**
     * Closes the statement and clears the private field indicating no more
     * processing should happen.
     */
    private def closeStatement(): Unit = {
      if ps == null then
        return
      try {
        ps.close()
      } finally {
        /* Clear the state so we won't attempt to close it again. */
        ps = null
      }
    }
  }
}
