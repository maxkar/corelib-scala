package io.github.maxkar
package sql.result

import java.sql.ResultSet

import scala.language.dynamics

/**
 * One row of the database output. Has useful methods for accessing named
 * argument (or, more specifically, binding prepared statement and name together).
 *
 * @param rs result set that would be accessed for fields.
 */
final class Row(rs: ResultSet) extends Dynamic:
  /**
   * Selects a field from the row and returns the data that could be
   * used by a database integration dialect to extract a value of an expected type.
   */
  def selectDynamic(field: String): RowQuery =
    new RowQuery(rs, field)
end Row
