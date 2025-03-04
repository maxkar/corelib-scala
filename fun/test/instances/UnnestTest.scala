package io.github.maxkar
package fun.instances

import fun.typeclass.Monad

/** Tests for the Unnest monad. */
final class UnnestTest extends org.scalatest.funsuite.AnyFunSuite {
  /** Some "operation". */
  def getV[M[_]: Monad](x: Int): M[Int] =
    Monad.pure(x)


  /** Some function - just add 1. */
  def add1[M[_]: Monad](x: Int): M[Int] =
    Monad.pure(x + 1)


  /** Some tail recursion with aggregate. */
  def badTailRec[M[_]: Monad](steps: Int, agg: Int): M[Int] =
    getV(steps) flatMap { v =>
      val nagg = agg + v
      if steps == 0 then Monad.pure(nagg) else badTailRec(steps - 1, nagg)
    }


  test("Bad tail recursion causes stack overflow on Identity monad") {
    import Identity.given

    assertThrows[StackOverflowError] {
      badTailRec(10000, 0)
    }
  }


  test("Bad tail recursion does not cause stack overflow for Unnest") {
    import Unnest.given

    val res = Unnest.run(badTailRec(10000, 0))
    assert(res === (10000 / 2) * 10001)
  }


  test("Long chain of flatmaps don't cause Stack Overflow") {
    import Unnest.given

    var base: Unnest[Int] = Monad.pure(0)
    val iterCount = 1000000

    for
      x <- 1 to iterCount
    do
      base = base.flatMap(add1)

    assert(iterCount === Unnest.run(base))
  }
}
