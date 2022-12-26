package io.github.maxkar
package sql.query

import sql.connection.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet

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


  /** Performs an update of the database and returns number of rows updated. */
  def updateCount()(using Connection): Int =
    withPreparedStatement { _.executeUpdate() }


  /**
   * Performs an update on the database and returns number of rows updated. This
   * is a synonym for `updateCount`.
   */
  inline def update()(using Connection): Int = updateCount()


  /** Performs an update on the database and returns if any rows were actually updated. */
  def updatedAny()(using Connection): Boolean =
    updateCount() > 0


  /** Performs an update on the database and returns if NO rows were actually updated. */
  def updatedNothing()(using Connection): Boolean =
    updateCount() == 0


  /**
   * Runs the (select) query and returns the result according to the provided result parser.
   *
   * @param parser result set parser. Usually created by the Result class (for specifying
   *   multiplicity and getting adapter) and set user-defined extraction methods.
   */
  def select[T](parser: ResultSet => T)(using Connection): T =
    withPreparedStatement { statement =>
      val rs = statement.executeQuery()
      try
        parser(rs)
      finally
        rs.close()
    }


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


  /** Executes the callback on the prepared statement defined by this query. */
  private def withPreparedStatement[T](cb: PreparedStatement => T)(using Connection): T =
    Connection.withPreparedStatement(getQueryText()) { ps =>
      setParameters(ps, 1)
      cb(ps)
    }
end Query



object Query:
  /**
   * Creates a "raw" query - text that would be interpreted "as-is", without any
   * safety measures. This method should be used with trusted input only.
   */
  def raw(text: String): Query =
    new Query(Seq(text), Seq.empty)
end Query
