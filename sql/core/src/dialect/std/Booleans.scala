package io.github.maxkar
package sql.dialect.std

import java.sql.Types
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement

import sql.query.Fragment
import sql.result.RowField
import sql.result.RowExtractor


/** Conversions and operations for booleans. */
object Booleans {
  /** Null setter of boolean. */
  private val nullOfBoolean = nullOfType(Types.BOOLEAN)


  /** Simple (positional) boolean extractor. */
  val boolean: RowExtractor[Boolean] =
    new RowExtractor[Boolean] {
      override def apply(rs: ResultSet): Boolean =
        ensureNotNull(rs.getBoolean(1), rs)
    }


  /** Simple (positional) nullable boolean extractor. */
  val optBoolean: RowExtractor[Option[Boolean]] =
    new RowExtractor[Option[Boolean]] {
      override def apply(rs: ResultSet): Option[Boolean] =
        getNullable(rs.getBoolean(1), rs)
    }


  /** Conversion between boolean and fragment. */
  given boolean2Fragment: Conversion[Boolean, Fragment] with {
    override def apply(x: Boolean): Fragment =
      new Fragment {
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int = {
          statement.setBoolean(startIndex, x)
          startIndex + 1
        }
      }
  }


  /** Conversion between row field and boolean. */
  given rowField2Boolean: Conversion[RowField, Boolean] with {
    override def apply(x: RowField): Boolean =
      ensureNotNull(x.resultSet.getBoolean(x.fieldName), x.resultSet)
  }



  /** Converter between optional (nullable) boolean and query fragment. */
  given optBoolean2Fragment: Conversion[Option[Boolean], Fragment] with {
    override def apply(x: Option[Boolean]): Fragment =
      x match {
        case None => nullOfBoolean
        case Some(x) => boolean2Fragment(x)
      }
  }


  /** Conversion between row field and optional (nullable) boolean. */
  given rowField2OptBoolean: Conversion[RowField, Option[Boolean]] with {
    override def apply(x: RowField): Option[Boolean] =
      getNullable(x.resultSet.getBoolean(x.fieldName), x.resultSet)
  }


  /** Checks that the value is not null. */
  private inline def ensureNotNull(value: Boolean, rs: ResultSet): Boolean = {
    if !value && rs.wasNull() then
      throw new SQLException("Got null for non-nullable boolean field")
    value
  }


  /** Extracts nullable value. */
  private inline def getNullable(value: Boolean, rs: ResultSet): Option[Boolean] =
    if !value && rs.wasNull() then
      None
    else
      Some(value)
}
