package io.github.maxkar
package sql.query

import java.sql.PreparedStatement

/**
 * A "fragment" of a query it knows how to build a query text and then provide
 * parameters to the PreparedStatement.
 */
trait Fragment:
  /** Appends the fragment's text to the string builder. */
  def appendQuery(sb: StringBuilder): Unit


  /**
   * Sets parameters in the prepared statement that was generated using the text
   * provided by this fragment.
   *
   * @param statement statement that was prepared with this fragment's participation.
   * @param startIndex index of the first parameter that should be set by this fragment.
   * @return index of the next parameter to set (i.e. index of the parameter next to
   *   the last parameter filled by this fragment).
   */
  def setParameters(statement: PreparedStatement, startIndex: Int): Int
end Fragment



object Fragment:

  /**
   * Creates a "raw" fragment - text that would be interpreted "as-is", without any
   * safety measures. This method should be used with trusted input only.
   */
  def raw(text: String): Fragment =
    new Fragment:
      override def appendQuery(sb: StringBuilder): Unit =
        sb.append(text)

      override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
        startIndex
    end new
  end raw
end Fragment
