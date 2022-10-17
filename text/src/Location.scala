package io.github.maxkar
package text

/**
 * Location inside some block of text (input field, text file, etc...).
 * @param offset zero-based offset of the "next" character in the input.
 * @param line one-based line in the input.
 * @param column one-based indexf on the "next character" column.
 */
final case class Location(
      offset: Int,
      line: Int,
      column: Int,
    )
