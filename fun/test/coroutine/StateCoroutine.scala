package io.github.maxkar
package fun.coroutine


/** Implementation of the "State" monad using coroutine and some tests of the combination. */
final class StateCoroutine extends org.scalatest.funsuite.AnyFunSuite:
  import StateCoroutine._


  /** Returns double of the state. */
  private val getDouble: Routine[Int] =
    for
      v <- getState
    yield v * 2


  /** Triples the state. */
  private val tripleState: Routine[Unit] =
    getState flatMap { x => setState(3 * x) }


  /** Some operation(s) on the state. */
  val doSomething: Routine[Int] =
    for
      v1 <- getState
      _ <- setState(v1 + 1)
      v2 <- getDouble
      _ <- tripleState
      v3 <- getState
    yield v1 * 10000 + v2 * 100 + v3


  test("Running coroutine-based state works as expected") {
    assert(203 === runState(0, doSomething))
    assert(10406 === runState(1, doSomething))
    assert(20609 === runState(2, doSomething))
    assert(30812 === runState(3, doSomething))
  }

end StateCoroutine


object StateCoroutine:
  /** Coroutine module. */
  val module = new Coroutine[StateSus]
  import module._

  export module.given
  export module.Routine


  /** State suspension. Hard-coded state type. */
  enum StateSus[T]:
    case Read(cont: Int => module.RunResult[T])
    case Write(value: Int, cont: Unit => module.RunResult[T])
  end StateSus


  /** Monad/operation for getting the current state. */
  val getState: Routine[Int] =
    module.suspend(new Suspender[Int] {
      override def encode[V](continue: Int => RunResult[V]): StateSus[V] =
        StateSus.Read(continue)
    })


  /** Monad/operation for setting the current state. */
  def setState(v: Int): Routine[Unit] =
    module.suspend(new Suspender[Unit] {
      override def encode[V](continue: Unit => RunResult[V]): StateSus[V] =
        StateSus.Write(v, continue)
    })


  /**
   * Runs the state monad (using coroutine module as the base).
   */
  def runState[T](state: Int, routine: Routine[T]): T =
    /* Stackless (non-recursive) loop here. Just for fun and consistency. */
    var curState = state
    var nextRes = module.run(routine)
    while true do
      nextRes match
        case Coroutine.RunResult.Suspended(StateSus.Read(otherCont)) =>
          nextRes = otherCont(curState)
        case Coroutine.RunResult.Suspended(StateSus.Write(v, c)) =>
          curState = v
          nextRes = c(())
        case Coroutine.RunResult.Finished(x) => return x
      end match
    end while
    throw new Error("Please stop reaching unreacheable code")
  end runState

end StateCoroutine
