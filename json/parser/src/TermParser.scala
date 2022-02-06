package io.github.maxkar
package json.parser

import fun.Monad

/**
 * Parser for simple term value (true, false, boolean).
 * @tparam M operation type.
 * @tparam J JSON value representation type.
 *
 * @param term string representation of the term to parse.
 * @param parserInput input stream/input capabilities.
 * @param jsonFactory json element factory.
 */
private class TermParser[M[_]: Monad, J](
      term: String,
      parserInput: input.ParserInput[M],
      jsonFactory: factory.TermFactory[M, J]
    ):

  /**
   * State of the stateful scanning.
   * The first argument in the next char to read (non-negative) or a representation
   * of the invalid character encountered `-errorOffset - 1` (negative).
   * The second argument is peer's state captured at the token start.
   */
  private type ScanState = (Int, jsonFactory.State)


  /** Parses the `term` from the input stream. */
  def parse: M[J] =
    for
      peerState <- jsonFactory.begin
      (pos, peerState) <- parserInput.statefulScan((0, peerState), updateState)
      res <-
        pos match
          case x if x == term.length => jsonFactory.end(peerState)
          case x if x < 0 => jsonFactory.badInput(peerState, term, ~x)
          case x => jsonFactory.unexpectedEnd(peerState, term, x)
    yield
      res


  /** Scans the input and updates the state based on the provided characters. */
  private def updateState(buffer: CharSequence, state: ScanState): (input.ConsumerStatus, ScanState) =
    var (pos, peerState) = state
    var offset = 0
    while pos < term.size && offset < buffer.length && term(pos) == buffer.charAt(offset) do
      pos += 1
      offset += 1

    if pos == term.size then
      (input.ConsumerStatus.Finished(offset), (pos, peerState))
    else if offset == buffer.length then
      (input.ConsumerStatus.NeedMoreInput, (pos, peerState))
    else
      (input.ConsumerStatus.Finished(offset), (~pos, peerState))
  end updateState

end TermParser
