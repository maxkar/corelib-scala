package io.github.maxkar
package text.input.typeclass

/** A "character input" typeclass that works as an implicit source of characters. */
trait Input[M[_]] {
  /**
   * Reads some characters into the `target` array.
   *
   * @param target target array to fill.
   * @param targetStart a first position to fill.
   * @param targetEnd end of the target section (the charater at the
   *   `targetEnd` location should not be overwritten).
   * @return number of characters read or negative value if end of input was reached.
   */
  def read(target: Array[Char], targetStart: Int, targetEnd: Int): M[Int]
}


object Input {
  inline def read[M[_]](
        target: Array[Char],
        targetStart: Int,
        targetEnd: Int
      )(using
        i: Input[M]
      ): M[Int] =
    i.read(target, targetStart, targetEnd)


  inline def read[M[_]](
        target: Array[Char]
      )(using
        i: Input[M]
      ): M[Int] =
    i.read(target, 0, target.length)
}
