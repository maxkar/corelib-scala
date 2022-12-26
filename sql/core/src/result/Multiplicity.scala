package io.github.maxkar
package sql.result

import java.sql.ResultSet
import java.sql.SQLException

import scala.collection.mutable.ArrayBuffer

/** Multiplicity specifiers and combinators for result set parsing. */
object Multiplicity:
  /**
   * Selects exactly one row and raises an exception if there is less than
   * or more than one row.
   */
  def one[T](row: RowExtractor[T]): ResultSet => T =
    rs => {
      if !rs.next() then
        throw new SQLException("No rows found, expected one")
      val res = row(rs)
      if rs.next() then
        throw new SQLException("More than one row found, expected one")
      res
    }


  /**
   * Select at most one row from the result. Result may return 0 or 1 row.
   * If more rows are returned, then exception is raised.
   */
  def atMostOne[T](row: RowExtractor[T]): ResultSet => Option[T] =
    rs => {
      if !rs.next() then
        None
      else {
        val res = row(rs)
        if rs.next() then
          throw new SQLException("More than one row found, expected at most one")
        Some(res)
      }
    }


  /** Synonym for `atMostOne`. */
  inline def optional[T](row: RowExtractor[T]): ResultSet => Option[T] =
    atMostOne(row)


  /** Selects many rows from the database. */
  def many[T](row: RowExtractor[T]): ResultSet => Seq[T] =
    rs => {
      val res = new ArrayBuffer[T]
      while rs.next do
        res += row(rs)
      res.toSeq
    }

end Multiplicity
