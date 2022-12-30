package io.github.maxkar
package sql.query

import java.sql.SQLException


/**
 * A batch update exception in the internal batch system.
 *
 * @param updateCounts update counts (up to but excluding elementOffset) for the previous statements.
 * @param batchOffset offset of the first element in the batch that raised an exception.
 * @param batchSize number of elements sent to DB in the current batch. Or the number of elements that
 *   were successfully registered in the batch before an issue happened (in this case is encodes an
 *   index of the element on which configuration failed).
 * @param statementText text of the statement that was used in the batch update statement.
 * @param cause the underlying exception that caused the batch to fail. It may be BatchUpdateException
 *   if the issue happened during the actual batch submission. But it may be a general SQLException
 *   that happened while the batch was prepared.
 */
final class BatchException(
    val updateCounts: Seq[Int],
    val batchOffset: Int,
    val batchSize: Int,
    val statementText: String,
    val cause: SQLException)
  extends SQLException(cause.getMessage(), cause)
