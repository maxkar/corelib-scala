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

  /** Flat map node (i.e. function that has to be applied to the pure value). */
  private final case class FlatMap[S, +R](base: Unnest[S], fn: S => Unnest[R]) extends Unnest[R]


  /** Implementation of the monad on the Unnest computation. */
  given unnestMonad: Monad[Unnest] with
    override def pure[T](v: T): Unnest[T] = Pure(v)

    override def bind[S, R](v: Unnest[S], fn: S => Unnest[R]): Unnest[R] =
      FlatMap(v, fn)
  end unnestMonad


  /** Performs the computation and returns the value evaluated by the monad. */
  def run[T](v: Unnest[T]): T =
    var cur: Unnest[T] = v
    while true do
      cur match
        case Pure(v) => return v
        case FlatMap(Pure(v), fn) => cur = fn(v)
        case FlatMap(FlatMap(v, fn1), fn) =>
          /* FlatMap rewrite is "lazy" - it is done at the time of evaluation and
           * not at the time of bind. This is crucial for doing constant-stack evaluation
           * as long chains of `x.flatMap.flatMap.flatMap` are transformed differently.
           * "Eager" conversion (on bind) is **left-associative**. The conversions are as follows:

           * ```
           * a.flatMap(f1).flatMap(f2).flatMap(f3).flatMap(f4) ==>
           * a.flatMap(y1 => f1(y1).flatMap(f2)).flatMap(f3).flatMap(f4) ==>
           * a.flatMap(y2 => (y1 => f1(y1).flatMap(f2))(y2).flatMap(f3)).flatMap(f4) ==>
           * a.flatMap(y3 => (y2 => (y1 => f1(y1).flatMap(f2))(y2).flatMap(f3))(y3).flatMap(f4))
           * ```
           * Eventually the `y3 => ...` function gets its argument and starts the evaluation.
           * Due to the shape of the function, it immediately applies the `y2 => ...` function. That
           * function in turn applies `y1 => ...` function. This chain uses stack and could grow
           * arbitrarily long thus causing StackOverflow upon evaluation.
           *
           * "Lazy" conversion works as follows:
           * ```
           * a.flatMap(f1).flatMap(f2).flatMap(f3).flatMap(f4) ==>
           * a.flatMap(f1).flatMap(f2).flatMap(y3 => f3(y3).flatMap(f4)) ==>
           * a.flatMap(f1).flatMap(y2 => f2(y2).flatMap(y3 => f3(y3).flatMap(f4))) ==>
           * a.flatMap(y1 => f1(y1).flatMap(y2 => f2(y2).flatMap(y3 => f3(y3).flatMap(f4))))
           * ```
           * The shape is much better. When `y1 => ...` function is evaluated, it only invokes
           * the `f1` function and then constructs a new FlatMap(f1(y1), y2 => ...). It does not
           * invoke the `y2 => ...` function inside the `y1 => ...` function but instead lets
           * the next iteration of unwind to perform function invocation. This way the constant
           * stack requirement for evaluation is satisfied.
           */
          cur = FlatMap(v, x => FlatMap(fn1(x), fn))
      end match
    end while
    throw new Error("Unreacheable code reached")
  end run
end Unnest
