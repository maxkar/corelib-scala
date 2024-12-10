package io.github.maxkar
package sql.dialect

import java.sql.PreparedStatement

import sql.query.Fragment


/** Standard dialect - the very basic and universal things. */
object standard {
  export std.Ints.given
  export std.Ints.{int, optInt}
  export std.Longs.given
  export std.Longs.{long, optLong}
  export std.Booleans.given
  export std.Booleans.{boolean, optBoolean}
  export std.Strings.given
  export std.Strings.{string, optString}
  export std.BigDecimals.given
  export std.BigDecimals.{bigDecimal, optBigDecimal}
  export std.ByteArrays.given
  export std.ByteArrays.{byteArray, optByteArray}


  /** Setter for null value. Uses NULL text and no parameters. */
  val nullValue: Fragment =
    new Fragment {
      override def appendQuery(sb: StringBuilder): Unit =
        sb.append("NULL")
      override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
        startIndex
    }
}
