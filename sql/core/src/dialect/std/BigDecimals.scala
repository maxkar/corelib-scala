package io.github.maxkar
package sql.dialect.std

import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement

import sql.query.Fragment
import sql.result.RowField
import sql.result.RowExtractor


object BigDecimals {
  /** Simple (positional) big decimal extractor. */
  val bigDecimal: RowExtractor[BigDecimal] =
    new RowExtractor[BigDecimal] {
      override def apply(rs: ResultSet): BigDecimal =
        ensureNotNull(rs.getBigDecimal(1))
    }


  /** Simple (positional) nullable big decimal extractor. */
  val optBigDecimal: RowExtractor[Option[BigDecimal]] =
    new RowExtractor[Option[BigDecimal]] {
      override def apply(rs: ResultSet): Option[BigDecimal] =
        getNullable(rs.getBigDecimal(1))
    }


  /** Conversion between big decimal and fragment. */
  given bigDecimal2Fragment: Conversion[BigDecimal, Fragment] with {
    override def apply(x: BigDecimal): Fragment =
      new Fragment {
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int = {
          statement.setBigDecimal(startIndex, x.bigDecimal)
          startIndex + 1
        }
      }
  }


  /** Conversion between optional big decimal and fragment. */
  given optBigDecimal2Fragment: Conversion[Option[BigDecimal], Fragment] with {
    override def apply(x: Option[BigDecimal]): Fragment =
      new Fragment {
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int = {
          x match {
            case Some(x) => statement.setBigDecimal(startIndex, x.bigDecimal)
            case None => statement.setBigDecimal(startIndex, null)
          }
          startIndex + 1
        }
      }
  }


  /** Conversion between row field and big decimal. */
  given rowField2BigDecimal: Conversion[RowField, BigDecimal] with {
    override def apply(x: RowField): BigDecimal =
      ensureNotNull(x.resultSet.getBigDecimal(x.fieldName))
    end apply
  }


  /** Conversion between row field and optional big decimal. */
  given rowField2OptBigDecimal: Conversion[RowField, Option[BigDecimal]] with {
    override def apply(x: RowField): Option[BigDecimal] =
      getNullable(x.resultSet.getBigDecimal(x.fieldName))
  }


  /** Checks that the value is not null. */
  private inline def ensureNotNull(value: java.math.BigDecimal): BigDecimal = {
    if value == null then
      throw new SQLException("Got null for non-nullable big decimal field")
    BigDecimal(value)
  }


  /** Extracts nullable value. */
  private inline def getNullable(value: java.math.BigDecimal): Option[BigDecimal] =
    if value == null then
      None
    else
      Some(BigDecimal(value))
}
