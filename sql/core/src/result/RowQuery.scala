package io.github.maxkar
package sql.result

import java.sql.ResultSet

/**
 * A query for a specific field on the prepared statement. It captures both
 * prepared statement and field name but does not know the expected type.
 * The actual type is supposed to be extracted by a specific database dialect.
 */
case class RowQuery(resultSet: ResultSet, fieldName: String)
