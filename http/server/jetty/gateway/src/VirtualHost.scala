package io.github.maxkar
package http.server.jetty.gateway

import org.eclipse.jetty.server.{Handler => JettyHandler}

/**
 * One "virtual host" on the same server - a collection of related handlers
 * that serve the same "logical aspect". For example, a group of business handlers
 * or a set of infrastructure-related processors.
 * @param port port on which all the handlers should be accessible.
 * @param handlers mapping from "path prefix" on which the handler would be accessible
 *   to the jetty processing logic. Most of the handlers are provided by related
 *   `jetty-server-*` libraries.
 */
final case class VirtualHost(port: Int, handlers: (String, JettyHandler)*)
