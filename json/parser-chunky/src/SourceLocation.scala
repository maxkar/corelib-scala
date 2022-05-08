package io.github.maxkar
package json.parser.chunky

/**
 * Location in the source (Json) stream/file. It tracks both "relative offset"
 * and "next character" positions. For example the very first character in the file
 * has offset=0, line=1 and column=1 parameters.
 *
 * @param offset zero-based offest of the position (number of characters "processed"
 *   until the current location).
 * @param line one-based number of the "current" line in the source stream.
 * @param column one-based offset of the "next" character.
 */
final case class SourceLocation(offset: Int, line: Int, column: Int):
  /**
   * An offset of the "previous" character in the input stream.
   *
   * The primary use-case is generating good human-readable messages. An application
   * may want to display location of some data in the input structure (for example,
   * "this number was defined on line 5, characters 30-35"). The parser tracks locations
   * "around" the characters (i.e. the start will be column 30 but end will be column 36),
   * not the inputs. However the `end` will give "human-readable" representation of the
   * _inclusive_ input (i.e. ```end.prev``` will have colunm 35, which is what user needs).
   */
  def prev: SourceLocation = SourceLocation(offset - 1, line, column - 1)

end SourceLocation
