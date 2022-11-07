package io.github.maxkar
package http.server.api

/**
 * Reader that puts read data into the array of bytes.
 */
trait ByteArrayReader[M[_]]:
  /**
   * Reads bytes into the array.
   * @param target target array to read.
   * @param start start offset to read to.
   * @param length maximal length of the data to read.
   * @return number of bytes read or negative value if end of stream reached.
   */
  def read(target: Array[Byte], start: Int, length: Int): M[Int]


  /** Reads the data from the start of the array and up to its length. */
  def read(target: Array[Byte]): M[Int] =
    read(target, 0, target.length)
end ByteArrayReader
