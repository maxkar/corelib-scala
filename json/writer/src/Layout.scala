package io.github.maxkar
package json.writer

import fun.typeclass.Applicative
import fun.typeclass.Monad


/** A basic layout of the JSON output of the stream `S`. */
trait Layout[M[_], -S] {
  /** Layout for an array. */
  val arrayLayout: Arrays.Layout[M, S]

  /** Layout for an object. */
  val objectLayout: Objects.Layout[M, S]

  /** Layout for the nested values (nested arrays or objects). */
  def nested: Layout[M, S]
}


object Layout {
  /** Compact layout - no spaces. */
  final class Compact[M[_]: Applicative] extends Layout[M, Any] {
    override val arrayLayout = Arrays.Layout.Compact()
    override val objectLayout = Objects.Layout.Compact()
    override def nested: Layout[M, Any] = this
  }


  /** Indent layout with the given indent value. */
  final class Indent[M[_]: Monad, -S: DefaultWriter.In[M]] private(
        base: Int,
        step: Int
      ) extends Layout[M, S] {
    override val arrayLayout = Arrays.Layout.Indent(base + step, base)
    override val objectLayout = Objects.Layout.Indent(base + step, base)
    override def nested: Layout[M, S] = new Indent(base + step, step)
  }


  object Indent {
    def apply[M[_]: Monad, S: DefaultWriter.In[M]](step: Int): Layout[M, S] =
      new Indent(0, step)
  }
}
