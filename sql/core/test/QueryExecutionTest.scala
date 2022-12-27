package io.github.maxkar
package sql

import connection.AutocommitConnection

import syntax.*
import syntax.given

import dialect.standard.*
import dialect.standard.given

import scala.language.implicitConversions

/**
 * Test than encompases the very basic features:
 *  * SQL Query interpolation
 *  * Result (row) parsing
 */
final class QueryExecutionTest extends DbTest:
  import QueryExecutionTest.*


  dbTest("Basic Query Test")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ){ conn ?=>
    assert(
      sql"""
        INSERT INTO test(id, value)
        VALUES 1, 'Hello'
      """.updatedAny()
    )


    val startIdx = 2
    val word2 = "Cruel"
    var word3 = "World"
    assert(
      sql"""
        INSERT INTO test(id, value)
        VALUES
          (${startIdx}, ${word2}),
          (${startIdx+1}, ${word3})
      """.updateCount() === 2
    )


    val greetings =
      sql"""
        SELECT value FROM test ORDER BY ID
      """ select many(string)

    assert(greetings === Seq("Hello", word2, word3))

    val greetLines =
      sql"""
        SELECT id, value FROM test ORDER BY ID
      """ select many(greetLine)

    assert(greetLines === Seq(GreetLine(1, "Hello"), GreetLine(2, "Cruel"), GreetLine(3, "World")))
  }


  /** Parses a greeting line. */
  private def greetLine(row: Row): GreetLine =
    GreetLine(row.id, row.value)
end QueryExecutionTest


object QueryExecutionTest:
  case class GreetLine(id: Int, word: String)
end QueryExecutionTest
