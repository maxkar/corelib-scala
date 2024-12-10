package io.github.maxkar
package sql.dialect.std

import java.sql.Types
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement

import sql.query.Fragment
import sql.result.RowField
import sql.result.RowExtractor


/** Conversions and operations for longs. */
object Longs {
  /** Null setter of long. */
  private val nullOfLong = nullOfType(Types.BIGINT)


  /** Simple (positional) long extractor. */
  val long: RowExtractor[Long] =
    new RowExtractor[Long] {
      override def apply(rs: ResultSet): Long =
        ensureNotNull(rs.getLong(1), rs)
    }


  /** Simple (positional) nullable string extractor. */
  val optLong: RowExtractor[Option[Long]] =
    new RowExtractor[Option[Long]] {
      override def apply(rs: ResultSet): Option[Long] =
        getNullable(rs.getLong(1), rs)
    }


  /** Conversion between long and fragment. */
  given long2Fragment: Conversion[Long, Fragment] with {
    override def apply(x: Long): Fragment =
      new Fragment {
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int = {
          statement.setLong(startIndex, x)
          startIndex + 1
        }
      }
  }


  /** Conversion between row field and long. */
  given rowField2Long: Conversion[RowField, Long] with {
    override def apply(x: RowField): Long =
      ensureNotNull(x.resultSet.getLong(x.fieldName), x.resultSet)
  }


  /** Converter between optional (nullable) long and query fragment. */
  given optLong2Fragment: Conversion[Option[Long], Fragment] with {
    override def apply(x: Option[Long]): Fragment =
      x match {
        case None => nullOfLong
        case Some(x) => long2Fragment(x)
      }
  }


  /** Conversion between row field and optional (nullable) long. */
  given rowField2OptLong: Conversion[RowField, Option[Long]] with {
    override def apply(x: RowField): Option[Long] =
      getNullable(x.resultSet.getLong(x.fieldName), x.resultSet)
  }


  /** Checks that the value is not null. */
  private inline def ensureNotNull(value: Long, rs: ResultSet): Long = {
    if value == 0 && rs.wasNull() then
      throw new SQLException("Got null for non-nullable long field")
    value
  }


  /** Extracts nullable value. */
  private inline def getNullable(value: Long, rs: ResultSet): Option[Long] = {
    if value == 0 && rs.wasNull() then
      None
    else
      Some(value)
  }
}
