package io.github.maxkar
package json.parser

/**
 * A simplest (minimal) input abstraction for the parser. It is good for
 * quickly (relatively to other methods) testing assumptions and feasibility
 * of input models.
 *
 * @tparam M type of the "input operation".
 */
trait BasicInput[M[_]]:
  /**
   * Statefully processes the input until no more could be consumed by the
   * `step` function or until end of input is reached.
   *
   * The function is similar to the `reduce` operation on the input chunks with
   * an additional ability to terminate processing early.
   *
   * The pseudocode for the implementation is
   *
   * {{{
   *   var currentState = initialState
   *   var stop = false
   *   while (!eof() && !stop) {
   *      var input = readInput()
   *      var (command, newState) = step(input, currentState)
   *      currentState = newState
   *      input match {
   *        case ConsumerStatus.NeedMoreInput => ()
   *        case ConsumerStatus.Finished(pos) =>
   *          returnUnprocessed(input.subSequence(pos, input.length))
   *          stop = true
   *      }
   *   }
   *
   *   return currentState
   * }}}
   *
   * The implementation should be adjusted to work with the desired input model M.
   *
   * @tparam S type of the consumer's state.
   * @param initialState initial operation state.
   * @param step function to advance the `current` state based on the input data.
   *   It returns the output (if more input is expected in the returned state) and
   *   the new state (which should be passed as an input to the next operation).
   * @return resulting state at either the end of the input or at the moment when
   *   the `step` function returned ConsumerStatus.Finished (whatever happens first).
   */
  def statefulScan[S](initialState: S, step: (CharSequence, S) => (ConsumerStatus, S)): M[S]

end BasicInput
