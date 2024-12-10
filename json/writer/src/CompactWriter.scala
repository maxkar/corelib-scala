package io.github.maxkar
package json.writer

import text.output.Stream

import fun.typeclass.Monad

/** Implementation of the "compact output" writer. */
private final class CompactWriter[M[_]: Monad, T](
      classifier: Values.ValueClassifier[T],
    )
    extends Writer[M, T] {

  override def write(value: T, stream: Stream[M]): M[Unit] =
    new CompactWriter.InstanceWriter(classifier, stream).write(value)
}


private object CompactWriter {
  /** Writer for one value instance. */
  private[writer] final class InstanceWriter[M[_]: Monad, T](
        classifier: Values.ValueClassifier[T],
        stream: Stream[M]
      )
      extends Values.ValueCallback[T, M[Unit]] {

    /** Writes single value into the stream. */
    def write(value: T): M[Unit] = classifier.classifyValue(value, this)

    /** Element writer, for compatibility with the iterator/object APIs. */
    def writeElt(value: T, stream: Stream[M]): M[Unit] =
      classifier.classifyValue(value, this)

    override def boolean(v: Boolean): M[Unit] =
      Literals.writeBoolean(v, stream)

    override def nullValue(): M[Unit] =
      Literals.writeNull(stream)

    override def string(v: CharSequence): M[Unit] =
      Strings.writeString(v, stream)

    override def number(representation: CharSequence): M[Unit] =
      stream.write(representation)

    override def array(iter: Iterator[T]): M[Unit] =
      Arrays.writeAll(Arrays.Whitespaces.nothing, writeElt, iter, stream)

    override def unorderedObject(iter: Iterator[(String, T)]): M[Unit] =
      Objects.writeAll(Objects.Whitespaces.nothing, writeElt, iter, stream)
  }
}
