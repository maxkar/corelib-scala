package io.github.maxkar
package fun.coroutine

/**
 * Result of attempting to execute a coroutine or part of it.
 * @tparam CallTarget a type constructor for describing "invocation" of
 *   some process/algorithm. The result type of the algorithm is encoded
 *   by the argument to the CallTarget.
 * @tparam T type of the coroutine result.
 */
enum Flow[CallTarget[_], +T] {
  /** The coroutine completed with the given result. */
  case Done(v: T)

  /**
   * The execution routine reached the point where the `target` procedure
   * (or method) should be executed.
   *
   * @param target procedure to be executed.
   * @param continue function to continue coroutine execution after the
   *   `target` was invoked.
   */
  case Call[CallTarget[_], T, R](
        target: CallTarget[R],
        contitue: R => Flow[CallTarget, T]
      ) extends Flow[CallTarget, T]
}
