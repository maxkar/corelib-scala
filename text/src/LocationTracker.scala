package io.github.maxkar
package text

/** A simple utility for calculating current location in the file. */
final class LocationTracker {
  /** Offset (in characters) in the stream. */
  private var offset: Int = 0

  /** Current line. */
  private var line: Int = 1

  /**
   * Current character we are looking at (i.e. the character that would be
   * read from input). */
  private var column: Int = 1

  /**
   * Indicates that the last observed character was Carriage Return.
   * A consequent Line Feed character would not cause line/column to be updated.
   */
  private var afterCR = false


  /** Returns location of the "next" character to be read. */
  def location(): Location = Location(offset = offset, line = line, column = column)


  /** Updates location based on the character observed. */
  def update(char: Int): Unit = {
    offset += 1
    updateLineAndColumn(char)
  }


  /** Updates location based on multiple characters observed. */
  def update(data: Array[Char], start: Int, end: Int): Unit = {
    offset += end - start

    var ptr = start
    while ptr < end do {
      updateLineAndColumn(data(ptr))
      ptr += 1
    }
  }


  /** Updates line and column positions. */
  private def updateLineAndColumn(char: Int): Unit =
    char match {
      case '\n' if afterCR =>
        /* Just ignore LF in the "CRLF" sequence. */
        afterCR = false
      case '\n' =>
        newLine()
      case '\r' =>
        newLine()
        afterCR = true
      case _ =>
        column += 1
        afterCR = false
    }


  /** Moves position to the next line. */
  private def newLine(): Unit = {
    line += 1
    column = 1
  }
}
