package io.github.maxkar
package json.parser.v3

/** A typeclass that allows looking at the stream without consuming its data. */
trait Peek[M[_], -S] {
  extension (stream: S) {
    /**
     * Looks at the charater at the `offset` from the current position
     * and returns its value. Returns negative value if `offset` points
     * behind end of the stream.
     *
     * @param offset offset to look the character at. It should be non-negative
     *   and less than `Peek#MAX_LOOK_AHEAD_DISTANCE`
     */
    def peek(offset: Int): M[Int]
  }
}


object Peek {
  /** Maximum number of characters that could be previewed. */
  val MAX_LOOK_AHEAD_DISTANCE = 6

  type In[M[_]] = [T] =>> Peek[M, T]
  type Of[T] = [M[_]] =>> Peek[M, T]
}
