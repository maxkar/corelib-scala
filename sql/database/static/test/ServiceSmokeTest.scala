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


  test("Basic service operations - running queries and stopping the DB") {
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
      val threadFactory = new CountingThreadFactory()
      val taskQueue = new FifoTaskProvider()

      val config = new Configuration(
        connection = Configuration.Connection.LoginPassword(s"jdbc:hsqldb:hsql://localhost:${dbPort}/${dbName}", "SA", ""),
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
    finally
      server.shutdown()
  }
end ServiceSmokeTest
