package io.github.maxkar
package sql

import java.sql.DriverManager
import java.sql.{Connection => JdbcConnection}

/** Utilities for working with temporary database. */
object TempDb:
  /** Runs a code block with the database initialized in some specific configuration. */
  def withTestDb[T](initCommands: String*)(cb: JdbcConnection => T): T =
    withTestConnection { conn =>
      val stmt = conn.createStatement()
      try
        initCommands.foreach(stmt.execute)
      finally
        stmt.close()

      cb(conn)
    }


  /**
   * Runs a code block on a temporary in-memory database.
   */
  def withTestConnection[T](cb: JdbcConnection => T): T =
    import org.hsqldb.Server

    val nullStream = new java.io.PrintWriter(NullWriter)
    val server = new Server()
    server.setDatabaseName(0, "mydb")
    server.setDatabasePath(0, "mem:mydb")
    server.setLogWriter(nullStream)
    server.setErrWriter(nullStream)
    server.start()

    try
      Class.forName("org.hsqldb.jdbc.JDBCDriver")
      val conn = DriverManager.getConnection("jdbc:hsqldb:hsql://localhost/mydb", "SA", "")
      try
        cb(conn)
      finally
        conn.close()
    finally
      server.shutdown()
  end withTestConnection
end TempDb
