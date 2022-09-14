package io.github.maxkar
package json


/**
 * Json output module that converts "DOM-like" model into the "writeable" (serialised) form.
 *
 * A "writer" is just an iterator over the output fragments. This may feel a bit unusual at first.
 * However such design decouples output formatting from stream handling. A basic implementation will
 * just pump the contents of the iterator into the java.io.Writer. An advanced implementation may
 * integrate with buffered asynchronous IO and only write fragments chunk by chunk, as the output
 * buffer becomes ready.
 */
package object writer {
  /**
   * A "json writer" - iterator over output fragments. These should be pumped into
   * an actual output stream.
   */
  type Writer = Iterator[CharSequence]
}
