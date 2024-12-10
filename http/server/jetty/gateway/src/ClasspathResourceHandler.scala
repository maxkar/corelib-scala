package io.github.maxkar
package http.server.jetty.gateway

import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.ResourceHandler

import org.eclipse.jetty.util.resource.Resource

object ClasspathResourceHandler {

  /**
   * Creates a new classpath resource that will serve data from the
   * specific classpath.
   *
   * @param basePath base directory from which all the paths would be resolved.
   */
  def apply(basePath: String): Handler = {
    val resource = Resource.newClassPathResource(basePath)
    val handler = new ResourceHandler()
    handler.setBaseResource(resource)
    handler.setAcceptRanges(true)
    handler.setDirectoriesListed(false)
    handler.setEtags(true)
    handler.setRedirectWelcome(false)
    handler.setDirAllowed(false)
    handler
  }
}
