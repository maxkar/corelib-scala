package io.github.maxkar
package http.server.jetty.qos

import http.server.api.Route
import http.server.api.Errors
import http.server.api.NegotiableErrors

import http.server.jetty.gateway.Gateway
import http.server.jetty.gateway.VirtualHost

import java.util.concurrent.ThreadFactory


/** Basic test for the QoS support. */
final class QosTest extends org.scalatest.funsuite.AnyFunSuite {

  test("Superuser request's get priority") {
    val activeThreads = new java.util.concurrent.atomic.AtomicInteger()

    val module =
      Module[Int](
        errors = NegotiableErrors(Seq.empty, Errors.Minimal),
        knownMethods = Route.defaultMethods,
        defaultQos = 0,
        threadFactory = new ThreadFactory {
          override def newThread(x: Runnable): Thread = {
            val t = new Thread(new Runnable() {
              override def run(): Unit = {
                activeThreads.incrementAndGet()
                try
                  x.run()
                finally
                  activeThreads.decrementAndGet()
              }
            })
            t.setName("Handler thread")
            t.setDaemon(true)
            t
          }
        },
        workThreads = 1,
        sensor = Sensor.PrintStack,
      )

    import module.given
    val handler = new SampleHandler(isRoot => module.setQos(if isRoot then 1 else 5))


    val js =
      Gateway.create(
        VirtualHost(8080, "/api" -> Handler(module, handler.handle))
      )

    js.start()

    val lock = new Object()
    var buf = new scala.collection.mutable.ArrayBuffer[(Boolean, Int)]

    val threads =
      for
        (x, idx) <- Seq(false, false, false, true).zipWithIndex
      yield {
        val t = new Thread() {
          override def run(): Unit = {
            val ret = query(x)
            assert(ret === "Hello, world!")
            lock synchronized {
              buf += ((x, idx))
            }
          }
        }
        t.start()
        Thread.sleep(100)
        t
      }

    Thread.sleep(200)
    assert(module.activeRequestCount === 4)
    assert(module.liveRequestCount === 1)
    assert(module.queuedRequestCount === 3)

    Thread.sleep(2000)
    assert(module.activeRequestCount === 3)
    assert(module.liveRequestCount === 1)
    assert(module.queuedRequestCount === 2)

    threads.foreach(_.join())

    assert(module.activeRequestCount === 0)
    assert(module.liveRequestCount === 0)
    assert(module.queuedRequestCount === 0)

    val res = buf.toSeq
    assert(res === Seq((false, 0), (true, 3), (false, 1), (false, 2)))

    js.stop()
    module.stop()

    assert(activeThreads.get() === 0)
    assert(module.activeRequestCount === 0)
    assert(module.liveRequestCount === 0)
    assert(module.queuedRequestCount === 0)
  }


  /** Runs the query to the server. */
  private def query(admin: Boolean): String = {
    val url = new java.net.URI("http://localhost:8080/api").toURL()
    val conn = url.openConnection().asInstanceOf[java.net.HttpURLConnection]
    conn.setDoOutput(false)
    conn.setDoInput(true)
    conn.addRequestProperty("X-Admin", if admin then "true" else "false")
    conn.connect()

    try {
      val is = conn.getInputStream()
      try {
        val ret = new String(is.readAllBytes(), "UTF-8")
        ret
      } finally {
        is.close()
      }
    } finally {
      conn.disconnect()
    }
  }
}
