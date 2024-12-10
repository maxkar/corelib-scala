package io.github.maxkar
package sql

import java.sql.DriverManager
import java.sql.{Connection => JdbcConnection}

/** Utilities for working with temporary database. */
object TempDb {
  /**
   * An ID of the "next" database to run.
   * Tests are run in parallel. We can prevent them from doing so. Or we can
   * just give every test its own DB. We go with the second approach here.
   */
  private val dbIndex = new java.util.concurrent.atomic.AtomicInteger()

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
    val dbName = s"mydb${dbIndex.incrementAndGet()}"

    Class.forName("org.hsqldb.jdbc.JDBCDriver")
    val conn = DriverManager.getConnection(s"jdbc:hsqldb:mem:${dbName};shutdown=true", "SA", "")
    try
      cb(conn)
    finally
      conn.close()
  end withTestConnection
}
