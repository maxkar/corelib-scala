package io.github.maxkar
package http.server.jetty.qos

import http.server.api.ResourceCleaner

/** Cleaner of the resource - knows how to clean-up the data. */
private final class ResourceCleanerImpl[Md[_]](op: Md[Unit]) extends ResourceCleaner[Md] {
  override def clean(): Md[Unit] = op
}
