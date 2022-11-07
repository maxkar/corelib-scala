package io.github.maxkar
package http.server.api

/**
 * Reader that puts read data into the array of characters.
 */
trait CharArrayReader[M[_]]:
  /**
   * Reads characters into the array.
   * @param target target array to read.
   * @param start start offset to read to.
   * @param length maximal length of the data to read.
   * @return number of characters read or negative value if end of stream reached.
   */
  def read(target: Array[Char], start: Int, length: Int): M[Int]


  /** Reads the data from the start of the array and up to its length. */
  def read(target: Array[Char]): M[Int] =
    read(target, 0, target.length)
end CharArrayReader
