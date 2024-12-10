package io.github.maxkar
package fun.typeclass

/**
 * Collector of multiple M-like operations into one M operation producing a sequence.
 * This transformation may be implemented using fold over the monad, but monads may have
 * monad-specific implementation (faster or using less stack).
 */
trait Collect[M[_]] {
  /*
   * Collects multiple operations into one operation returning the sequence of results.
   */
  def collect[T](items: Seq[M[T]]): M[Seq[T]]
}


object Collect {
  /**
   * A nice invocation syntax that may be used to launch multiple operations:
   * {{{
   *   for {
   *     res <- Collect(doA(), doB())
   *   } yield ...
   * }}}
   */
  inline def apply[M[_], T](ops: M[T]*)(using col: Collect[M]): M[Seq[T]] =
    col.collect(ops)

  /**
   * Stand-alone version of collect. Reduces typing in simple cases like:
   * {{{
   *   def doSomething[M[_]: Collect](items: Seq[Int]): M[Res] =
   *     for {
   *       results <- Collect.seq(items.map(loadOneItem))
   *     } yield mkRes(results)
   * }}}
   *
   */
  inline def seq[M[_], T](items: Seq[M[T]])(using col: Collect[M]): M[Seq[T]] =
    col.collect(items)
}
