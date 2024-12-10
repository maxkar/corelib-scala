package io.github.maxkar
package sql.dialect.std

import java.sql.PreparedStatement
import sql.query.Fragment

/**
 * Creates null of the given type. It does not provide a null literal but instead
 * uses a parameter placeholder and invokes a `PreparedStatement.setNull` method.
 * This way the statement generated is similar to the "proper type" converters which
 * is essentiall for batch processing that relies on the statements being exactly
 * the same to be included into the query.
 */
def nullOfType(sqlType: Int): Fragment =
  new Fragment {
    override def appendQuery(sb: StringBuilder): Unit =
      sb.append('?')

    override def setParameters(statement: PreparedStatement, startIndex: Int): Int = {
      statement.setNull(startIndex, sqlType)
      startIndex + 1
    }
  }

