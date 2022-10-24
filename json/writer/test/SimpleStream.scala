package io.github.maxkar
package json.writer

import text.output.Stream

import fun.instances.Identity

/**
 * Simple output stream implementation.
 */
private final class SimpleStream extends Stream[Identity]:
  /** Internal bufer for all the data written. */
  private val buffer = new StringBuilder()

  override def write(data: CharSequence): Unit =
    buffer.append(data)


  /** Returns all the output collected so far. */
  def data: String = buffer.toString()
end SimpleStream
