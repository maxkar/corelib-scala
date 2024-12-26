package io.github.maxkar
package text.v2.input

import fun.typeclass.Monad

import scala.annotation.targetName

/**
 * Reader from the type `T` using the execution monad `M`.
 */
trait Reader[M[_], T] {
  /**
   * Reads data from the `source` into the `target` and returns a number
   * of characters filled by the operation.
   * @param source source to read from.
   * @param target target array to fill.
   * @param targetStart starting offset of the target.
   * @param targetEnd ending offset of the target.
   * @return number of characters read or a negative value if end of the
   *   `source` was reached.
   */
  def read(source: T, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int]

  extension (t: T) {
    @targetName("readExt")
    inline def read(target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      this.read(t, target, targetStart, targetEnd)


    /** Reads the content of the reader as a string. */
    def readString()(using Monad[M]): M[String] = {
      val res = new StringBuilder()
      val buf = new Array[Char](1024)

      def rd(): M[String] =
        t.read(buf, 0, buf.length) <||| { readCount =>
          if (readCount <= 0) then
            Monad.pure(res.toString())
          else {
            res.appendAll(buf, 0, readCount)
            rd()
          }
        }
      rd()
    }
  }
}


object Reader {
  inline def read[M[_], T](
        source: T, target: Array[Char], targetStart: Int, targetEnd: Int
      )(using
        reader: Reader[M, T]
      ): M[Int] =
    reader.read(source, target, targetStart, targetEnd)
}
