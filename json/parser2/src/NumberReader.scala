package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Monad

/**
 * "Iterator-like" reader of the number. Reads number section by section
 * and returns `null` when there are no more parts to read.
 */
final class NumberReader[M[_]: Monad: Numbers.Errors] private[parser](
      private var state: Numbers.ParsingContinuation,
      stream: CharacterStream[M]
    ):
  /** Reads next part of the number. Returns `null` (within monad) if all the number was read. */
  def next(): M[CharSequence] =
    if state == null then
      Monad.pure(null)
    else
      state.continue(stream) map { (chars, newState) =>
        state = newState
        chars
      }
end NumberReader
