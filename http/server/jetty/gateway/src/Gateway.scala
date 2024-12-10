package io.github.maxkar
package http.server.jetty.gateway

import org.eclipse.jetty.server.{Server => JettyServer}
import org.eclipse.jetty.server.Connector
import org.eclipse.jetty.server.ServerConnector
import org.eclipse.jetty.server.HttpConfiguration
import org.eclipse.jetty.server.HttpConnectionFactory
import org.eclipse.jetty.server.handler.HandlerList
import org.eclipse.jetty.server.handler.ErrorHandler
import org.eclipse.jetty.server.handler.ContextHandler

import org.eclipse.jetty.util.thread.QueuedThreadPool

/**
 * The main entry point into launching jetty handlers.
 */
object Gateway {

  private def gzippableMethods = Set("GET", "POST", "PATCH")

  /**
   * Creates a configured jetty server but does not run in. The method
   * configures Jetty to use minimal amount of threads possible. It is
   * expected that all the handlers could effectively work in an unblocking
   * way (i.e. operate asynchronously or maintain their own pool).
   *
   * Multiple virtual hosts may have the same port specified. The handlers
   * defined by such hosts would be grouped together. The path prefix matching
   * would be performed in the order of the vhost and handler declaration.
   *
   * Consider the following scenario:
   * ```
   * val jetty =
   *   Gateway.create(
   *     VirtualHost(8080,
   *        "/api" -> handler1,
   *        "/" -> handler2,
   *     ),
   *     VirtualHost(8080,
   *       "/doc" -> handler3
   *     )
   *   )
   * ```
   *
   * In this configuration paths starting with `api` would be processed by the `handler1`
   * and all other paths would be processed by the `handler2`. No paths would be processed
   * by the `handler3` as the "/" prefix would be tried (and matched) before trying to
   * match the "/doc" path.
   *
   * @param hosts hosts to run on the single jetty server.
   */
  def create(hosts: VirtualHost*): JettyServer = {
    val contextHandlers =
      for
        vhost <- hosts
        vhostSpec = Array("@port" + vhost.port)
        (path, handler) <- vhost.handlers
      yield {
        val ctxHandler = new ContextHandler(path)
        ctxHandler.setHandler(handler)
        ctxHandler.setVirtualHosts(vhostSpec)
        ctxHandler.setAllowNullPathInfo(true)
        ctxHandler.setMaxFormContentSize(16 * 1024 * 1024)
        ctxHandler.setMaxFormKeys(511)
        ctxHandler
      }

    val ports =
      hosts.view
        .map(_.port)
        .toSet

    val server = new JettyServer(new QueuedThreadPool(3 + ports.size, 3, 60000))
    server.setHandler(new HandlerList(contextHandlers*))

    val errHandler = new ErrorHandler()
    errHandler.setShowMessageInTitle(false)
    errHandler.setShowServlet(false)
    errHandler.setShowStacks(false)
    server.setErrorHandler(errHandler)

    val connectors = ports.iterator.map(createConnector(server)).toArray
    server.setConnectors(connectors)
    server
  }


  /** Runs the server (blocks on it). */
  def run(vhosts: VirtualHost*): Unit = {
    val server = create(vhosts*)
    server.start()
    server.join()
  }


  /** Creates HTTP connector for a server running on the given port. */
  private def createConnector(server: JettyServer)(port: Int): Connector = {
    val conf = new HttpConfiguration()
    conf.setPersistentConnectionsEnabled(true)
    conf.setRelativeRedirectAllowed(true)
    conf.setSendDateHeader(false)
    conf.setSendServerVersion(false)
    conf.setSendXPoweredBy(false)

    val connFactory = new HttpConnectionFactory(conf)
    val conn = new ServerConnector(server, 1, 1, connFactory)
    conn.setName("port" + port)
    conn.setPort(port)
    conn
  }
}
