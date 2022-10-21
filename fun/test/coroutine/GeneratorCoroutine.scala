package io.github.maxkar
package fun.coroutine

import fun.typeclass.Monad


/** Implementation of the generator coroutine (yield-method from C#/Javascript). */
final class GeneratorCoroutine extends org.scalatest.funsuite.AnyFunSuite:
  import GeneratorCoroutine._

  /** Recursive (yep!) implementation of the sequence generator. */
  def genSeq(from: Int, to: Int, step: Int = 1): Routine[Unit] =
    doYield(from) flatMap { _ =>
      if from == to then Monad.pure(()) else genSeq(from + step, to, step)
    }


  /** Generates back-and-forth sequence. */
  def genBackAndForth(from: Int, to: Int, step: Int = 1): Routine[Unit] =
    for
      _ <- genSeq(from, to, step)
      _ <- genSeq(to, from, -step)
    yield ()


  test("Sequence generation works (smoke test)") {
    assert((Seq(1, 2, 3), ()) === runGenerator(genSeq(1, 3)))
    assert((Seq(3, 2, 1), ()) === runGenerator(genSeq(3, 1, -1)))
    assert((Seq(1, 2, 3, 3, 2, 1), ()) === runGenerator(genBackAndForth(1, 3, 1)))
  }


  test("Generating huge sequence(s) works") {
    val limit = 10000
    val fwd = (1 to limit).toSeq

    val rwd = fwd.reverse

    assert((fwd, ()) === runGenerator(genSeq(1, limit)))
    assert((fwd ++ rwd, ()) === runGenerator(genBackAndForth(1, limit)))
  }
end GeneratorCoroutine


object GeneratorCoroutine:
  /** Coroutine module. */
  case class GeneratorSus[T](v: Int, cont: Unit => Coroutine.RunResult[GeneratorSus, T])

  val module = new Coroutine[GeneratorSus]
  import module._

  export module.given
  export module.Routine


  /** Implementation of the "yield" operator. */
  def doYield(v: Int): Routine[Unit] =
    module.suspend(new Suspender[Unit] {
      override def encode[V](continue: Unit => RunResult[V]): GeneratorSus[V] =
        GeneratorSus(v, continue)
    })


  /** Runs the generator and returns both generated sequence and result. */
  def runGenerator[T](gen: Routine[T]): (Seq[Int], T) =
    var acc = new scala.collection.mutable.ArrayBuffer[Int]
    var nextRes = module.run(gen)

    /* Also "stackless" implementation. */
    while true do
      nextRes match
        case Coroutine.RunResult.Suspended(GeneratorSus(v, cont)) =>
          acc += v
          nextRes = cont(())
        case Coroutine.RunResult.Finished(v) =>
          return (acc.toSeq, v)
      end match
    end while
    throw new Error("Please stop reaching unreacheable code")
  end runGenerator

end GeneratorCoroutine
