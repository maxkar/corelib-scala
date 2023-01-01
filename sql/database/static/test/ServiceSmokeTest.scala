package io.github.maxkar
package sql.database.static

import sql.dialect.standard.given
import sql.dialect.standard.*
import sql.syntax.given
import sql.syntax.*

import java.util.concurrent.Semaphore

import scala.language.implicitConversions
import io.github.maxkar.sql.connection.AutocommitConnection

/**
 * Very basic smoke test for the connection service.
 */
final class ServiceSmokeTest extends org.scalatest.funsuite.AnyFunSuite:
  /**
   * An ID of the "next" database to run.
   * Tests are run in parallel. We can prevent them from doing so. Or we can
   * just give every test its own DB. We go with the second approach here.
   */
  private val dbIndex = new java.util.concurrent.atomic.AtomicInteger()


  /** Thread factory that counts threads. */
  final class CountingThreadFactory extends java.util.concurrent.ThreadFactory:
    private val threadCounter = new java.util.concurrent.atomic.AtomicInteger()

    /** Returns current number of active threads. */
    def activeThreadCount: Int = threadCounter.get()


    override def newThread(x: Runnable): Thread =
      val t = new Thread(new Runnable() {
        override def run(): Unit =
          threadCounter.incrementAndGet()
          try
            x.run()
          finally
            threadCounter.decrementAndGet()
      })
      t.setDaemon(true)
      t
    end newThread
  end CountingThreadFactory


  test("Attempt to connect to non-existend DB should throw an exception") {
    val threadFactory = new CountingThreadFactory()
    val taskQueue = new FifoTaskProvider()

    val config = new Configuration(
      connection = Configuration.Connection.LoginPassword("jdbc:hsqldb:hsql://localhost:88/thisisnotadb", "SA", ""),
      poolSize = 2,
      threadFactory = threadFactory,
    )

    assertThrows[Throwable] {
      Service(config, taskQueue, Sensor.noop)
    }
  }


  test("Basic service operations - running queries and stopping the DB") {
    withTempServer { dbUrl =>
      val threadFactory = new CountingThreadFactory()
      val taskQueue = new FifoTaskProvider()

      val config = new Configuration(
        connection = Configuration.Connection.LoginPassword(dbUrl, "SA", ""),
        poolSize = 2,
        threadFactory = threadFactory,
      )

      val svc = Service(config, taskQueue, Sensor.noop)
      Thread.sleep(400)
      assert(2 === threadFactory.activeThreadCount)

      val smp = new Semaphore(0)
      taskQueue.submit { conn =>
        val stmt = conn.jdbcConnection.createStatement()
        try
          stmt.execute(
            "CREATE TABLE test(id INTEGER NOT NULL PRIMARY KEY)"
          )
        finally
          stmt.close()
        smp.release()
      }
      smp.acquire()


      for i <- 1 to 10 do
        taskQueue.submit { conn =>
          given AutocommitConnection = conn
          sql"""INSERT INTO test(id) VALUES (${i})""".update()
          smp.release()
        }

      smp.acquire(10)
      var cnt = -1
      taskQueue.submit { conn =>
        given AutocommitConnection = conn
        cnt = sql"""SELECT count(*) FROM test""" select one(int)
        smp.release()
      }
      smp.acquire()
      assert(cnt === 10)

      svc.shutdown()
      assert(0 === threadFactory.activeThreadCount)
    }
  }


  test("DB Pool works in case of abnormal activity (exception thrown from the handler)") {
    withTempServer { dbUrl =>
      val threadFactory = new CountingThreadFactory()
      val taskQueue = new FifoTaskProvider()

      val config = new Configuration(
        connection = Configuration.Connection.LoginPassword(dbUrl, "SA", ""),
        poolSize = 2,
        threadFactory = threadFactory,
      )

      val svc = Service(config, taskQueue, Sensor.noop)
      Thread.sleep(400)
      assert(2 === threadFactory.activeThreadCount)

      val smp = new Semaphore(0)
      taskQueue.submit { conn =>
        val stmt = conn.jdbcConnection.createStatement()
        try
          stmt.execute(
            "CREATE TABLE test(id INTEGER NOT NULL PRIMARY KEY)"
          )
        finally
          stmt.close()
        smp.release()
      }
      smp.acquire()


      for i <- 1 to 10 do
        taskQueue.submit { conn =>
          given AutocommitConnection = conn
          sql"""INSERT INTO test(id) VALUES (${i})""".update()
          smp.release()
          /* This marks connection as potentially invalid, the connection should be validate
           * and re-used.
           */
          throw new Exception("Test")
        }

      for i <- 11 to 20 do
        taskQueue.submit { conn =>
          given AutocommitConnection = conn
          sql"""INSERT INTO test(id) VALUES (${i})""".update()
          smp.release()
          /* Similar to the previous, but this time connection is closed (not valid).
           * The condition should be detected and the connection should be reopened.
           */
          conn.jdbcConnection.close()
          throw new Exception("Test")
        }

      smp.acquire(20)
      var cnt = -1
      taskQueue.submit { conn =>
        given AutocommitConnection = conn
        cnt = sql"""SELECT count(*) FROM test""" select one(int)
        smp.release()
      }
      smp.acquire()
      assert(cnt === 20)

      svc.shutdown()
      assert(0 === threadFactory.activeThreadCount)
    }
  }


  /**
   * Creates a temp database server and invokes the callback with the DB URL provided.
   */
  private def withTempServer[T](callback: String => T): T =
    import org.hsqldb.Server

    val idx = dbIndex.incrementAndGet()
    val dbPort = idx + 7845
    val dbName = s"svcdb${idx}"
    val server = new Server()
    server.setDatabaseName(0, dbName)
    server.setDatabasePath(0, s"mem:${dbName}")
    server.setPort(dbPort)
    server.start()
    try
      callback(s"jdbc:hsqldb:hsql://localhost:${dbPort}/${dbName}")
    finally
      server.shutdown()
  end withTempServer
end ServiceSmokeTest
