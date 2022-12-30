package io.github.maxkar
package sql.dialect.std

import java.sql.Types
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement

import sql.query.Fragment
import sql.result.RowField
import sql.result.RowExtractor


/** Conversions and operations for ints. */
object Ints:
  /** Null setter of int. */
  private val nullOfInt = nullOfType(Types.INTEGER)


  /** Simple (positional) int extractor. */
  val int: RowExtractor[Int] =
    new RowExtractor[Int]:
      def apply(rs: ResultSet): Int =
        ensureNotNull(rs.getInt(1), rs)
    end new


  /** Simple (positional) nullable extractor. */
  val optInt: RowExtractor[Option[Int]] =
    new RowExtractor[Option[Int]]:
      def apply(rs: ResultSet): Option[Int] =
        getNullable(rs.getInt(1), rs)
    end new


  /** Conversion between int and fragment. */
  given int2Fragment: Conversion[Int, Fragment] with
    override def apply(x: Int): Fragment =
      new Fragment:
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
          statement.setInt(startIndex, x)
          startIndex + 1
        end setParameters
      end new
  end int2Fragment


  /** Conversion between row field and int. */
  given rowField2Int: Conversion[RowField, Int] with
    override def apply(x: RowField): Int =
      ensureNotNull(x.resultSet.getInt(x.fieldName), x.resultSet)
  end rowField2Int


  /** Converter between optional (nullable) int and query fragment. */
  given optInt2Fragment: Conversion[Option[Int], Fragment] with
    override def apply(x: Option[Int]): Fragment =
      x match
        case None => nullOfInt
        case Some(x) => int2Fragment(x)
      end match
    end apply
  end optInt2Fragment


  /** Conversion between row field and optional (nullable) int. */
  given rowField2OptInt: Conversion[RowField, Option[Int]] with
    override def apply(x: RowField): Option[Int] =
      getNullable(x.resultSet.getInt(x.fieldName), x.resultSet)
  end rowField2OptInt


  /** Checks that the value is not null. */
  private inline def ensureNotNull(value: Int, rs: ResultSet): Int =
    if value == 0 && rs.wasNull() then
      throw new SQLException("Got null for non-nullable int field")
    value
  end ensureNotNull


  /** Extracts nullable value. */
  private inline def getNullable(value: Int, rs: ResultSet): Option[Int] =
    if value == 0 && rs.wasNull() then
      None
    else
      Some(value)
  end getNullable
end Ints
