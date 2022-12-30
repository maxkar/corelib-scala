package io.github.maxkar
package sql.dialect.std

import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement

import sql.query.Fragment
import sql.result.RowField
import sql.result.RowExtractor


object ByteArrays:
  /** Simple (positional) byte array extractor. */
  val byteArray: RowExtractor[Array[Byte]] =
    new RowExtractor[Array[Byte]]:
      def apply(rs: ResultSet): Array[Byte] =
        ensureNotNull(rs.getBytes(1))
    end new


  /** Simple (positional) nullable byte array extractor. */
  val optByteArray: RowExtractor[Option[Array[Byte]]] =
    new RowExtractor[Option[Array[Byte]]]:
      def apply(rs: ResultSet): Option[Array[Byte]] =
        Option(rs.getBytes(1))
    end new


  /** Conversion between byte array and fragment. */
  given byteArray2Fragment: Conversion[Array[Byte], Fragment] with
    override def apply(x: Array[Byte]): Fragment =
      new Fragment:
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
          statement.setBytes(startIndex, x)
          startIndex + 1
        end setParameters
      end new
  end byteArray2Fragment


  /** Conversion between optional byte array and fragment. */
  given optByteArray2Fragment: Conversion[Option[Array[Byte]], Fragment] with
    override def apply(x: Option[Array[Byte]]): Fragment =
      new Fragment:
        override def appendQuery(sb: StringBuilder): Unit =
          sb.append('?')

        override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
          val v =
            x match
              case Some(x) => x
              case None => null
            end match
          statement.setBytes(startIndex, v)
          startIndex + 1
        end setParameters
      end new
  end optByteArray2Fragment


  /** Conversion between row field and byte array. */
  given rowField2ByteArray: Conversion[RowField, Array[Byte]] with
    override def apply(x: RowField): Array[Byte] =
      ensureNotNull(x.resultSet.getBytes(x.fieldName))
    end apply
  end rowField2ByteArray


  /** Conversion between row field and optional byte array. */
  given rowField2OptByteArray: Conversion[RowField, Option[Array[Byte]]] with
    override def apply(x: RowField): Option[Array[Byte]] =
      Option(x.resultSet.getBytes(x.fieldName))
  end rowField2OptByteArray


  /** Checks that the value is not null. */
  private inline def ensureNotNull(value: Array[Byte]): Array[Byte] =
    if value == null then
      throw new SQLException("Got null for non-nullable byte array field")
    value
  end ensureNotNull
end ByteArrays
