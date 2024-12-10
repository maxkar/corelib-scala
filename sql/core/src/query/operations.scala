package io.github.maxkar
package sql.query

import sql.connection.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet


/**
 * Additional operations like update and select that trigger actual database
 * calls. The functionality is split from the core query as it is "optional" for
 * the core query functionality. Some applications may like to use a custom
 * set of operations. For example, an application may want to use selects and
 * updates that integrate with an application telemetry thus requiring additional
 * sensors or context to be passed into the operation.
 */
object operations {
  extension (q: Query) {
    /** Performs an update of the database and returns number of rows updated. */
    def updateCount()(using Connection, Timeout): Int =
      withPreparedStatement { _.executeUpdate() }


    /**
     * Performs an update on the database and returns number of rows updated. This
     * is a synonym for `updateCount`.
     */
    inline def update()(using Connection, Timeout): Int =
       updateCount()


    /** Performs an update on the database and returns if any rows were actually updated. */
    def updatedAny()(using Connection, Timeout): Boolean =
      updateCount() > 0


    /** Performs an update on the database and returns if NO rows were actually updated. */
    def updatedNothing()(using Connection, Timeout): Boolean =
      updateCount() == 0


    /**
     * Runs the (select) query and returns the result according to the provided result parser.
     *
     * @param parser result set parser. Usually created by the Result class (for specifying
     *   multiplicity and getting adapter) and set user-defined extraction methods.
     */
    infix def select[T](parser: ResultSet => T)(using Connection, Timeout): T =
      withPreparedStatement { statement =>
        val rs = statement.executeQuery()
        try
          parser(rs)
        finally
          rs.close()
      }


    /** Executes the callback on the prepared statement defined by this query. */
    private def withPreparedStatement[T](
            cb: PreparedStatement => T
          )(using
            Connection, Timeout
          ): T =
      Connection.withPreparedStatement(q.getQueryText()) { ps =>
        ps.setQueryTimeout(implicitly[Timeout].timeoutSeconds)
        q.setParameters(ps, 1)
        cb(ps)
      }
  }
}
