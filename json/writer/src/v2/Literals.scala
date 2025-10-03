package io.github.maxkar
package json.writer.v2

import text.v2.output.Writer

/** Literal writers. */
object Literals {
  /** Writes the `true` literal. */
  def writeTrue[M[_], S: Writer.In[M]](stream: S): M[Unit] =
    stream.write("true")

  /** Writes the `false` literal. */
  def writeFalse[M[_], S: Writer.In[M]](stream: S): M[Unit] =
    stream.write("false")

  /** Writes the `null` literal. */
  def writeNull[M[_], S: Writer.In[M]](stream: S): M[Unit] =
    stream.write("null")

  /** Writes a boolean value. */
  def writeBoolean[M[_], S: Writer.In[M]](v: Boolean, stream: S): M[Unit] =
    if v then writeTrue(stream) else writeFalse(stream)
}
