package io.github.maxkar
package fun.coroutine

import v2.*

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
  /** State suspension. Hard-coded state type. */
  enum Action[T] {
    case Read extends Action[Int]
    case Write(value: Int) extends Action[Unit]
  }

  type Routine[T] = Coroutine[Action, T]


  /** Monad/operation for getting the current state. */
  val getState: Routine[Int] = Coroutine.call(Action.Read)


  /** Monad/operation for setting the current state. */
  def setState(v: Int): Routine[Unit] = Coroutine.call(Action.Write(v))


  /**
   * Runs the state monad (using coroutine module as the base).
   */
  def runState[T](state: Int, routine: Routine[T]): T = {
    /* Stackless (non-recursive) loop here. Just for fun and consistency. */
    var curState = state
    var proc = routine.run()
    while true do {
      proc match {
        case Flow.Call(Action.Read, cont) =>
          proc = cont(curState)
        case Flow.Call(Action.Write(v), cont) =>
          curState = v
          proc = cont(())
        case Flow.Done(x) => return x
      }
    }
    throw new Error("Please stop reaching unreacheable code")
  }
}
