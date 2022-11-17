package io.github.maxkar
package http.server.jetty.qos

import http.server.api.Route
import http.server.api.Errors
import http.server.api.NegotiableErrors

import http.server.jetty.gateway.Gateway
import http.server.jetty.gateway.VirtualHost
import http.server.jetty.qos.boundary.Completable

import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory

import scala.util.Success
import scala.util.Failure

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext

import scala.collection.mutable.ArrayBuffer

/** Checks the limit on number of requests in flight. */
final class RequestInFlightTest extends org.scalatest.funsuite.AnyFunSuite:
  test("Request limit works as expected") {
    val module =
      Module[Int](
        errors = NegotiableErrors(Seq.empty, Errors.Minimal),
        knownMethods = Route.defaultMethods,
        defaultQos = 0,
        threadFactory = new ThreadFactory {
          override def newThread(x: Runnable): Thread =
            val t = new Thread(x)
            t.setName("Handler thread")
            t.setDaemon(true)
            t
          end newThread
        },
        workThreads = 1,
        maxRequestsInFlight = 2,
        sensor = Sensor.PrintStack,
      )


    val futureEc = Executors.newSingleThreadExecutor()
    given ec: ExecutionContext = ExecutionContext.fromExecutor(futureEc)

    given Completable[Future] with
      override def onComplete[T](computation: Future[T], onSuccess: T => Unit, onFailure: Throwable => Unit): Unit =
        computation.onComplete {
          case Success(value) => onSuccess(value)
          case Failure(exception) => onFailure(exception)
        }
    end given

    val op = Promise[String]()

    import module.given
    val handler = new FutureHandler(op.future)

    val js =
      Gateway.create(
        VirtualHost(8080, "/api" -> Handler(module, handler.handle))
      )

    js.start()

    val lock = new Object()
    var buf = new scala.collection.mutable.ArrayBuffer[(Int, String)]

    val threads =
      Seq.tabulate(2) { idx =>
        val t = new Thread(new Runnable() {
          override def run(): Unit =
            val res = query()
            lock synchronized {
              buf += res
            }
        })
        t.start()
        t
      }

    Thread.sleep(200)
    assertThrows[java.io.IOException] {
      query()
    }
    op.success("Hello?")

    threads.foreach(_.join())
    assert(buf.toSeq === Seq((200, "Hello?"), (200, "Hello?")))

    assert(query() === (200, "Hello?"))

    js.stop()
    module.stop()
    futureEc.shutdown()

    assert(module.activeRequestCount === 0)
    assert(module.liveRequestCount === 0)
    assert(module.queuedRequestCount === 0)

  }


  /** Runs the query to the server. */
  private def query(): (Int, String) =
    val url = new java.net.URL("http://localhost:8080/api")
    val conn = url.openConnection().asInstanceOf[java.net.HttpURLConnection]
    conn.setDoOutput(false)
    conn.setDoInput(true)
    conn.connect()

    try
      val is = conn.getInputStream()
      try
        val code = conn.getResponseCode()
        val ret = new String(is.readAllBytes(), "UTF-8")
        (code, ret)
      finally
        is.close()
    finally
      conn.disconnect()
  end query
end RequestInFlightTest
