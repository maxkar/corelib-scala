package io.github.maxkar
package sql.query

import java.sql.PreparedStatement

/**
 * A single database query. It incapsulates all the required text and
 * parameter values. It also knows how to interact with database to execute
 * the query data.
 *
 * @param textFragments plain text framents (no query parameters, just text) that are
 *   intercalated by the fragments that may contain non-text parameters.
 * @param fragments non-text fragments that may contain both text and parameter
 *   data. These parameters are located between text fragments.
 */
final class Query(textFragments: Seq[String], fragments: Seq[Fragment]) extends Fragment:

  /**
   * Builds query text corresponding to this query. The query is "prepared statement"
   * query - it may contain parameter placeholders that would be populated upon execution.
   */
  def getQueryText(): String =
    val sb = new StringBuilder()
    appendQuery(sb)
    return sb.toString()
  end getQueryText


  override def appendQuery(sb: StringBuilder): Unit =
    val textIterator = textFragments.iterator
    sb.append(textIterator.next())

    val fragmentIterator = fragments.iterator
    while fragmentIterator.hasNext do
      fragmentIterator.next().appendQuery(sb)
      sb.append(textIterator.next())
    end while
  end appendQuery


  override def setParameters(statement: PreparedStatement, startIndex: Int): Int =
    var index = startIndex
    val fragmentIterator = fragments.iterator

    while fragmentIterator.hasNext do
      index = fragmentIterator.next().setParameters(statement, index)

    index
  end setParameters
end Query


object Query:
  /**
   * Creates a "raw" query - text that would be interpreted "as-is", without any
   * safety measures. This method should be used with trusted input only.
   */
  def raw(text: String): Query =
    new Query(Seq(text), Seq.empty)
end Query
