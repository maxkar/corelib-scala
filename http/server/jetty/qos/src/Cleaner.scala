package io.github.maxkar
package http.server.jetty.qos

/**
 * Some cleanup routine that could be regisetered for the request
 * and later cleaned either manually or automatically.
 * @param fn function used to cleanup the resource. Internally set to
 *   `null` when cleanup is done.
 */
private final class Cleaner(private var fn: () => Unit) {
  /**
   * Previous cleaner in the linked list of cleaners. Set to `null`
   * when first in the context or already cleaned.
   */
  private var prev: Cleaner = null

  /**
   * Next cleaner in the linked list of cleaners. Set to `null`
   * when last or is already cleaned.
   */
  private[qos] var next: Cleaner = null


  /**
   * Adds this cleaner to the (head) of the registered cleaners
   * for the given request.
   */
  private[qos] def register(context: RequestContext[?]): Unit = {
    next = context.cleaner
    if next != null then next.prev = this
    context.cleaner = this
  }


  /**
   * Removes this cleaner from the list of registered cleaners. The clean-up is
   * separate from actual cleaning as the request clean-up may be somewhat optimized
   * when the whole context is cleaned.
   */
  private[qos] def unregister(context: RequestContext[?]): Unit = {
    /* Already cleaned. */
    if fn == null then return

    if next != null then next.prev = prev

    if prev != null then
      prev.next = next
    else
      context.cleaner = next

    drop()
  }


  /**
   * Drops the links to prev/next nodes but does not attempt to "fix" other
   * nodes in the list. This is mostly useful for avoiding unnecessary object
   * retention if cleaner link is held for too long.
   */
  private[qos] def drop(): Unit = {
    prev = null
    next = null
  }


  /**
   * Runs the clean-up but does not unregister the cleaner. The caller should
   * first either `unregister` or `drop` this cleaner and only then invoke this
   * method.
   */
  private[qos] def performCleanup(): Unit =
    fn()
}
