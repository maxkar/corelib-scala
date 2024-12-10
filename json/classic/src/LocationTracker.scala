package io.github.maxkar
package json.classic

/** Tracker for input stream location. Used by JSON paser to create error messages. */
private final class LocationTracker {
  /** Absolute offset in the input stream. */
  private var charOffset = 0l

  /** Current column in the input document. */
  private var column = 1

  /** Current row in the input document. */
  private var row = 1

  /** True if after CR (i.e. in a potential CR/LF combination). */
  private var afterCR = false


  /** Updates the position depending on the character read from the stream. */
  def advance(c: Char): Unit = {
    charOffset += 1
    c match {
      case '\n' =>
        if !afterCR then
          toNextLine()
      case '\r' =>
        toNextLine()
      case _ =>
        column += 1
    }
    afterCR = c == '\r'
  }


  /** Returns string representation of the current location. */
  def getLocation(): String =
    s"(${row}:${column}, offset = ${charOffset})"


  /** Advances pointers to the next line. */
  private def toNextLine(): Unit = {
    row += 1
    column = 1
  }
}
