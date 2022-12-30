package io.github.maxkar
package sql.dialect.std

import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement

import sql.query.Fragment
import sql.result.RowField
import sql.result.RowExtractor


object Strings:
  /** Simple (positional) string extractor. */
  val string: RowExtractor[String] =
    new RowExtractor[String]:
      def apply(rs: ResultSet): String =
        ensureNotNull(rs.getString(1))
    end new


  /** Simple (positional) nullable string extractor. */
  val optString: RowExtractor[Option[String]] =
    new RowExtractor[Option[String]]:
      def apply(rs: ResultSet): Option[String] =
        Option(rs.getString(1))
    end new


  /** Conversion between string and fragment. */
  given string2Fragment: Conversion[String, Fragment] with
    override def apply(x: String): Fragment =
      new Fragment:
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
          statement.setString(startIndex, x)
          startIndex + 1
        end setParameters
      end new
  end string2Fragment


  /** Conversion between optional string and fragment. */
  given optString2Fragment: Conversion[Option[String], Fragment] with
    override def apply(x: Option[String]): Fragment =
      new Fragment:
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
          val v =
            x match
              case Some(x) => x
              case None => null
            end match

          statement.setString(startIndex, v)
          startIndex + 1
        end setParameters
      end new
  end optString2Fragment


  /** Conversion between row field and string. */
  given rowField2String: Conversion[RowField, String] with
    override def apply(x: RowField): String =
      ensureNotNull(x.resultSet.getString(x.fieldName))
  end rowField2String


  /** Conversion between row field and optional string. */
  given rowField2OptString: Conversion[RowField, Option[String]] with
    override def apply(x: RowField): Option[String] =
      Option(x.resultSet.getString(x.fieldName))
  end rowField2OptString


  /** Checks that the value is not null. */
  private inline def ensureNotNull(value: String): String =
    if value == null then
      throw new SQLException("Got null for non-nullable string field")
    value
  end ensureNotNull
end Strings
