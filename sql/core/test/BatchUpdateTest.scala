package io.github.maxkar
package sql

import syntax.*
import syntax.given

import dialect.standard.*
import dialect.standard.given

import scala.language.implicitConversions


/**
 * Test for batch update(s).
 */
final class BatchUpdateTest extends DbTest:
  dbTest("Default batch test")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL
      )"""
  ){ conn ?=>
    val updateCounts =
      Batch {
        for i <- 1 to 10 yield
          sql"""
            INSERT INTO test(id) values (${i})
          """
      }

    assert(10 === (sql"""SELECT count(*) FROM test""" select one(int)))
    assert(updateCounts === Seq.fill(10) {1})
  }


  dbTest("Batch split test (7+3)")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL
      )"""
  ){ conn ?=>
    val updateCounts =
      Batch.configure(batchSize = 7) {
        for i <- 1 to 10 yield
          sql"""
            INSERT INTO test(id) values (${i})
          """
      }

    assert(10 === (sql"""SELECT count(*) FROM test""" select one(int)))
    assert(updateCounts === Seq.fill(10) {1})
  }


  dbTest("Batch split test (5+5)")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL
      )"""
  ){ conn ?=>
    val updateCounts =
      Batch.configure(batchSize = 5) {
        for i <- 1 to 10 yield
          sql"""
            INSERT INTO test(id) values (${i})
          """
      }

    assert(10 === (sql"""SELECT count(*) FROM test""" select one(int)))
    assert(updateCounts === Seq.fill(10) {1})
  }
end BatchUpdateTest
