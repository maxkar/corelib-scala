package io.github.maxkar
package json.classic

import java.io.Reader

/** Input stream used by the json reader. */
private final class JsonStream(base: Reader):
  /** Location in the input stream. */
  private val tracker = LocationTracker()

  /** Next character to return. */
  private var next = base.read()


  /** Checks if the stream is at the end of the file. */
  def eof(): Boolean = next < 0


  /** Returns a next input character. Returns negative value if the stream is at its end. */
  def peek(): Int = next

  
  /** 
   * Reads a next character. 
   * @throws Json.Exception if the stream is at an end of file.
   */
  def read(): Char =
    if eof() then
      throw Json.Exception("Unexpected end of file at " + tracker.getLocation())
    val res = next.toChar
    tracker.advance(res)
    next = base.read()
    res
  end read


  /** Returs current position (location) in the input stream. */
  def location(): String =
    tracker.getLocation()

end JsonStream
