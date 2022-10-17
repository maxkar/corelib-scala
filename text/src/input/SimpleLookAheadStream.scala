package io.github.maxkar
package text.input

import fun.Applicative

/**
 * Implementation of the look-ahead stream that is based on the
 * existing (pre-read) character sequence.
 */
private[input] final class SimpleLookAheadStream[M[_]: Applicative](
      data: CharSequence,
    )
    extends LookAheadStream[M]:
  /** Current location in the stream. */
  private var ptr: Int = 0


  override def consume(count: Int): M[CharSequence] =
    val start = ptr
    ptr += count
    Applicative.pure(data.subSequence(start, ptr))
  end consume


  override def peek(minLength: Int): M[CharSequence] =
    Applicative.pure(data.subSequence(ptr, data.length()))


  override def skip(count: Int): M[Unit] =
    ptr += count
    Applicative.pure(())
  end skip


  override def releaseCharSequence(): M[Unit] =
    Applicative.pure(())

end SimpleLookAheadStream
