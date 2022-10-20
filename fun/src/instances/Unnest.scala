package io.github.maxkar
package fun.instances

import fun.typeclass.Monad

/**
 * An on-demand constant-stack evaluation monad. The monad has following properties
 * (encoded in its description):
 * * It is **on-demand**. The value should be exctracted by `run`-ning the execution.
 *   Running the execution multiple times will cause all relevant methods to be
 *   re-evaluated.
 * * It uses only a fixed amount of stack for monad-related operations (pure/map/flatMap).
 *   This only counts **stack required by the monad implementation**, functions that are
 *   passed to `map`/`flatMap` may still use arbitrary amount of stack.
 *
 * ## Purpose and Usage
 *
 * The main purpose of this monad is to avoid StackOverflow that may happen
 * in deeply-nested computations and with "naive" implementation of the monad. Consider
 * a typical for-comprehension code:
 *
 * ```
 * for
 *   a <- doA()
 *   b <- doB(a)
 *   c <- doC(b)
 * yield
 *   c + 1
 * ```
 *
 * It is desugared into the following:
 * ```
 * doA().flatMap { a =>
 *   doB(a).flatMap { b =>
 *     doC(b).map { c =>
 *       c + 1
 *     }
 *   }
 * }
 * ```
 *
 * The `flatMap` calls nests inside each other. A typical synchronous monad (Identity, Option, etc...)
 * will create at least three nested stack frames (one for each map/flatMap operation). This is because
 * the implementation just invokes the mapping function and during evaluation of that nested function
 * it invokes another mapping, which in turn causes more recursion.
 *
 * Another use-case that supported by this monad is handling (non-tail) recursive calls. Consider parsing
 * some tree-like recursive structure like JSON or XML. Value in such formats may be arbitrary nested. The
 * code may look like
 *
 * ```
 * def readValue: M[Value] =
 *   readValueType flatMap {
 *     case Array => readArray
 *     case Int => readInt
 *     ....
 *   }
 *
 * def readArray: M[Value] =
 *   readArrayAgg(new ArrayBuffer())
 *
 * def readArrayAgg(agg: ArrayBuffer): M[Value] =
 *   hasNextArrayValue flatMap {
 *     case true =>
 *       for
 *         value <- readValue
 *         agg += value
 *         ret <- readArrayAgg(agg)
 *       yield ret
 *     case false => Monad.pure(agg.toSeq)
 *   }
 * ```
 *
 * Note how readValue may inderectly call itself via readValue->readArray->readArrayAgg->readValue.
 * Such evaluation could cause stack-overflow if array nesting is too deep in the source stream.
 * Non-monad (sequential) is also likely to exhibit the same exception. The `Unnest` monad is able
 * to deal with this problem by avoiding excessive stack usage at the cost of some heap usage. Required
 * heap space (for this scenario) is proportional to the nesting depth but heap is usually much
 * less limited than the stack.
 *
 * The `Unnest` may also be useful with other monad transformers to reduce stack usage. For example,
 * M[T] = Unnest[Option[T]] will require less stack than the Option[T] (at the cost of some performance
 * and maybe heap usage).
 *
 *
 * ## Implementation
 *
 * The unnest monad is essentially an interpreter of abstract syntax tree that is build on monadic
 * operations like pure, map and flatMap. The system uses monad laws to rewrite some forms of nesting
 * thus reducing amount of the stack required for the computation.
 */
abstract sealed class Unnest[+T]:
  /** Evaluates value of this monad. */
  def run(): T = Unnest.run(this)
end Unnest

object Unnest:
  /** "Just a value" without any computation. */
  private final case class Pure[+T](value: T) extends Unnest[T]

  /**
   * Flat map node (i.e. function that has to be applied to the pure value).
   *
   * Note that the base operation is **always** pure value and not an
   * arbitrary `Unnest` instance. Dealing with arbitrary unnest may require extra
   * computational stack for evaluating it and this is what we are trying to avoid.
   * Monad instance for Unnest uses monad laws to re-write chains of
   * `a.flatMap(fn1).flatMap(fn2)` into `a.flatMap{x => fn1(x).flatMap(fn2)}` which
   * is supported by the computation and matches the predicate (`a` is either pure or
   * more rewrite rules are applied).
   */
  private final case class FlatMap[S, +R](base: S, fn: S => Unnest[R]) extends Unnest[R]


  /** Implementation of the monad on the Unnest computation. */
  given unnestMonad: Monad[Unnest] with
    override def pure[T](v: T): Unnest[T] = Pure(v)

    override def bind[S, R](v: Unnest[S], fn: S => Unnest[R]): Unnest[R] =
      v match
        case Pure(v) => FlatMap(v, fn)
        case FlatMap(base, fn1) => FlatMap(base, x => bind(fn1(x), fn))
      end match
    end bind
  end unnestMonad


  /** Performs the computation and returns the value evaluated by the monad. */
  def run[T](v: Unnest[T]): T =
    var cur: Unnest[T] = v
    while true do
      cur match
        case Pure(v) => return v
        case FlatMap(v, fn) => cur = fn(v)
      end match
    end while
    throw new Error("Unreacheable code reached")
  end run
end Unnest
