package io.github.maxkar
package sql.result

import java.sql.ResultSet

/** Extractor of one resultset row. */
trait RowExtractor[T] {
  /** Extracts one row of result from result set. */
  def apply(rs: ResultSet): T
}


object RowExtractor {

  /**
   * A handly conversion from a function (taking nice row and returning value)
   * into a row extractor.
   */
  given rowSyntaxParser[T]: Conversion[Row => T, RowExtractor[T]] with {
    override def apply(peer: Row => T): RowExtractor[T] =
      new RowExtractor[T] {
        override def apply(rs: ResultSet): T =
          peer(new Row(rs))
      }
  }
}
