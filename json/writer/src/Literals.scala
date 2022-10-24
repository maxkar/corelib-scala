package io.github.maxkar
package json.writer

import text.output.Stream

/** Literal writers. */
object Literals:
  /** Writes the `true` literal. */
  def writeTrue[M[_]](stream: Stream[M]): M[Unit] =
    stream.write("true")

  /** Writes the `false` literal. */
  def writeFalse[M[_]](stream: Stream[M]): M[Unit] =
    stream.write("false")

  /** Writes the `null` literal. */
  def writeNull[M[_]](stream: Stream[M]): M[Unit] =
    stream.write("null")

  /** Writes a boolean value. */
  def writeBoolean[M[_]](v: Boolean, stream: Stream[M]): Unit =
    if v then writeTrue(stream) else writeFalse(stream)
end Literals
