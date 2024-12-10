package io.github.maxkar
package json.sample.formatter.streaming

import fun.typeclass.Monad

import text.output.Stream

/** Regular indentation logic. */
final class RegularIndent[M[_]: Monad] extends Indent[M] {
  /** Indent level. */
  private var level = 0

  override def wrapAndIncrease(stream: Stream[M]): M[Unit] = {
    level += 1
    indent(stream)
  }

  override def wrapAndDecrease(stream: Stream[M]): M[Unit] = {
    level -= 1
    indent(stream)
  }

  override def wrapAndIndent(stream: Stream[M]): M[Unit] = {
    indent(stream)
  }

  override def indentKeyValuePair(stream: Stream[M]): M[Unit] =
    stream.write(" ")


  /** Indents at the current level. */
  private def indent(stream: Stream[M]): M[Unit] =
    stream.write("\n").flatMap { _ =>
      indentN(level, stream)
    }


  /** Intends `n` levels. */
  private def indentN(n: Int, stream: Stream[M]): M[Unit] =
    if n == 0 then
      Monad.pure(())
    else
      stream.write("  ").flatMap { _ =>
        indentN(n-1, stream)
      }
}
