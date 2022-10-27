package io.github.maxkar
package json.sample.formatter.streaming

import fun.typeclass.Applicative

import text.output.Stream

/** "No Indent" logic used for compact printing. */
final class NoIndent[M[_]: Applicative] extends Indent[M]:
  /** No-operation used here. */
  private val pass = Applicative.pure(())

  override def wrapAndIncrease(stream: Stream[M]): M[Unit] = pass

  override def wrapAndDecrease(stream: Stream[M]): M[Unit] = pass

  override def wrapAndIndent(stream: Stream[M]): M[Unit] = pass

  override def indentKeyValuePair(stream: Stream[M]): M[Unit] = pass
end NoIndent


