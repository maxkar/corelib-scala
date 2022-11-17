package io.github.maxkar
package http.server.jetty.qos

import http.server.api.Route
import http.server.api.Errors
import http.server.api.NegotiableErrors

import http.server.jetty.gateway.Gateway
import http.server.jetty.gateway.VirtualHost
import http.server.jetty.qos.boundary.Scheduled

import java.util.concurrent.ThreadFactory

import scala.collection.mutable.ArrayBuffer


/** Test for scheduled integration. */
final class ScheduledIntegrationTest extends org.scalatest.funsuite.AnyFunSuite:
  import ScheduledIntegrationTest.*


  test("Integration works as expected") {
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
        sensor = Sensor.PrintStack,
      )

    val requestQueue = new ArrayBuffer[Req[_]]()

    given Scheduled[Op, Int] with
      override def apply[T](
            operation: Op[T],
            qos: Int,
            ordinal: Long,
            onSuccess: T => Unit,
            onFailure: Throwable => Unit,
          ): Unit =
        requestQueue synchronized {
          requestQueue += Req(operation, qos, onSuccess)
        }
    end given

    import module.given
    val handler = new DeferredQosHandler(
        module.setQos,
        x => Wrap(x),
    )

    val js =
      Gateway.create(
        VirtualHost(8080, "/api" -> Handler(module, handler.handle))
      )

    js.start()

    val lock = new Object()
    var buf = new scala.collection.mutable.ArrayBuffer[(Int, String)]

    val threads =
      Seq.tabulate(4) { idx =>
        val t = new Thread(new Runnable() {
          override def run(): Unit =
            val res = query(idx)
            lock synchronized {
              buf += ((idx, res))
            }
        })
        t.start()
        t
      }

    Thread.sleep(100)
    requestQueue synchronized {
      assert(requestQueue.length === 4)
    }

    assert(module.activeRequestCount === 4)
    assert(module.liveRequestCount === 0)
    assert(module.queuedRequestCount === 0)

    reply(requestQueue, 2, 10)
    Thread.sleep(100)
    reply(requestQueue, 1, 5)
    Thread.sleep(50)
    reply(requestQueue, 3, 50)
    Thread.sleep(50)
    reply(requestQueue, 0, 42)

    threads.foreach(_.join())
    val bsq = buf.toSeq

    assert(bsq === Seq(
      2 -> "2:10", 1 -> "1:5", 3 -> "3:50", 0 -> "0:42",
    ))

    js.stop()
    module.stop()

    assert(module.activeRequestCount === 0)
    assert(module.liveRequestCount === 0)
    assert(module.queuedRequestCount === 0)
  }


  /** Replies to the element at the given position. */
  private def reply(queue: Iterable[Req[_]], serial: Int, resp: Int): Unit =
    val elem = queue.find { e => e.qos == serial }.get

    elem match {
      case Req(Wrap(v), _, cb) =>
        assert(v === serial)
        cb(resp)
    }
  end reply


  /** Runs the query to the server. */
  private def query(id: Int): String =
    val url = new java.net.URL("http://localhost:8080/api")
    val conn = url.openConnection().asInstanceOf[java.net.HttpURLConnection]
    conn.setDoOutput(false)
    conn.setDoInput(true)
    conn.addRequestProperty("X-Id", id.toString)
    conn.connect()

    try
      val is = conn.getInputStream()
      try
        val ret = new String(is.readAllBytes(), "UTF-8")
        ret
      finally
        is.close()
    finally
      conn.disconnect()
  end query
end ScheduledIntegrationTest


object ScheduledIntegrationTest:
  /** Just some value for doing schedule integration. */
  private abstract sealed class Op[T]

  private case class Wrap(x: Int) extends Op[Int]

  private case class Req[T](op: Op[T], qos: Int, cb: T => Unit)
end ScheduledIntegrationTest
