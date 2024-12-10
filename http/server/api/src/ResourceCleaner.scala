package io.github.maxkar
package http.server.api

/** Cleaner of resources associated with the given request. */
trait ResourceCleaner[M[_]] {
  /** Cleans the resource. */
  def clean(): M[Unit]
}
