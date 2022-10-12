package io.github.maxkar
package json.parser

import json.parser.input.CharacterStream

import fun.Monad


/**
 * Reader of the string data.
 * Pull interface that reads data chunk by chunk.
 */
final class StringReader[M[_]: Monad, S <: CharacterStream[M]](
      stream: S
    )(using
      errs: Strings.Errors[M, S]
    ):

  /** Inidcator on how to start reading. */
  private var isStart = true

  /** Inidcates if we processed everything. */
  private var isEnd = false

  /** Reads next part of the number. Returns `null` (within monad) if all the number was read. */
  def next(): M[CharSequence] =
    if isEnd then
      return Monad.pure(null)
    val op =
      if isStart then
        isStart = false
        Strings.startString(stream)
      else
        Strings.continueString(stream)
    op map { (data, hasMore) =>
      isEnd = !hasMore
      data
    }
  end next

end StringReader
