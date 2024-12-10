package io.github.maxkar
package fun.coroutine


/** Implementation of the "State" monad using coroutine and some tests of the combination. */
final class StateCoroutine extends org.scalatest.funsuite.AnyFunSuite {
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
}


object StateCoroutine {
  /** Coroutine module. */
  val module = new Coroutine[StateSus]
  import module._

  export module.given
  export module.Routine

  /** State suspension. Hard-coded state type. */
  enum StateSus[T] {
    case Read extends StateSus[Int]
    case Write(value: Int) extends StateSus[Unit]
  }


  /** Monad/operation for getting the current state. */
  val getState: Routine[Int] = module.suspend(StateSus.Read)


  /** Monad/operation for setting the current state. */
  def setState(v: Int): Routine[Unit] = module.suspend(StateSus.Write(v))


  /**
   * Runs the state monad (using coroutine module as the base).
   */
  def runState[T](state: Int, routine: Routine[T]): T = {
    /* Stackless (non-recursive) loop here. Just for fun and consistency. */
    var curState = state
    var proc = routine
    while true do {
      module.run(proc) match {
        case Coroutine.RunResult.Suspended(StateSus.Read, cont) =>
          proc = cont(curState)
        case Coroutine.RunResult.Suspended(StateSus.Write(v), cont) =>
          curState = v
          proc = cont(())
        case Coroutine.RunResult.Finished(x) => return x
      }
    }
    throw new Error("Please stop reaching unreacheable code")
  }
}
