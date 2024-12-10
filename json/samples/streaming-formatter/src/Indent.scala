package io.github.maxkar
package json.sample.formatter.streaming

import text.output.Stream

/** Indent logic. */
trait Indent[M[_]] {
  /** Wraps the line (if supported), increases the indend by one and puts the indented line. */
  def wrapAndIncrease(stream: Stream[M]): M[Unit]

  /** Wraps the line (if supported), decreasing the indend by one and puts the indented line. */
  def wrapAndDecrease(stream: Stream[M]): M[Unit]

  /** Wraps the line (if supported), and puts the current indent. */
  def wrapAndIndent(stream: Stream[M]): M[Unit]

  /** Indents key-value pair (after the key-value separator). */
  def indentKeyValuePair(stream: Stream[M]): M[Unit]
}
