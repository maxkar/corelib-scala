package io.github.maxkar
package sql

import syntax.*
import syntax.given

import dialect.standard.*
import dialect.standard.given

import scala.language.implicitConversions

/**
 * Test than validates basic transaction logic.
 */
final class TransactionTest extends DbTest:
  dbTest("Basic Transaction Test")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ){ conn ?=>
    conn.allOrNothing { tx ?=>
      sql"""
        INSERT INTO test VALUES(1, 'Hello')
      """.update()

      tx.setRollbackOnly()
      sql"""
        INSERT INTO test VALUES(2, 'World')
      """.update()
    }

    assert(0 === (sql"""SELECT count(*) FROM test""" select one(int)))
  }


  dbTest("Nested Transaction Test")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ) { conn ?=>
    conn.allOrNothing { tx ?=>
      sql"""
        INSERT INTO test VALUES(1, 'Hello')
      """.update()


      tx.allOrNothing { subtx ?=>
        sql"""
          INSERT INTO test VALUES(2, 'World')
        """.update()
        subtx.setRollbackOnly()
      }
    }

    assert(1 === (sql"""SELECT count(*) FROM test""" select one(int)))
  }


  dbTest("Transactions are rolled back on exception")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ){ conn ?=>
    try
      conn.allOrNothing { tx ?=>
        sql"""
          INSERT INTO test VALUES(1, 'Hello')
        """.update()

        sql"""
          INSERT INTO test VALUES(2, 'World')
        """.update()

        throw new Exception("Rollback!")
      }
      fail("Exception expected")
    catch
      case _: Throwable => ()

    assert(0 === (sql"""SELECT count(*) FROM test""" select one(int)))
  }



  dbTest("Nested Transaction are rolled back on exceptions")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ) { conn ?=>
    conn.allOrNothing { tx ?=>
      sql"""
        INSERT INTO test VALUES(1, 'Hello')
      """.update()


      try
        tx.allOrNothing { subtx ?=>
          sql"""
            INSERT INTO test VALUES(2, 'World')
          """.update()
          throw new Exception("Rollback!")
        }
        fail("Exception expected")
      catch
        case _: Throwable => ()
    }

    assert(1 === (sql"""SELECT count(*) FROM test""" select one(int)))
  }


  dbTest("Rollback only does not conflict with exceptions (nested tx)")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ) { conn ?=>
    conn.allOrNothing { tx ?=>
      sql"""
        INSERT INTO test VALUES(1, 'Hello')
      """.update()


      try
        tx.allOrNothing { subtx ?=>
          sql"""
            INSERT INTO test VALUES(2, 'World')
          """.update()
          subtx.setRollbackOnly()
          throw new Exception("Rollback!")
        }
        fail("Exception expected")
      catch
        case _: Throwable => ()
    }

    assert(1 === (sql"""SELECT count(*) FROM test""" select one(int)))
  }


  dbTest("Callbacks are invoked on successfull commit")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ) { conn ?=>
    var callCount = 0

    conn.allOrNothing { tx ?=>
      sql"""
        INSERT INTO test VALUES(1, 'Hello')
      """.update()

      tx.onCommit(() => callCount += 1)
    }

    assert(1 === callCount)
  }


  dbTest("Callbacks are not invoked on rolled back commit (explicit rollback)")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ) { conn ?=>
    var callCount = 0

    conn.allOrNothing { tx ?=>
      sql"""
        INSERT INTO test VALUES(1, 'Hello')
      """.update()

      tx.onCommit(() => callCount += 1)
      tx.setRollbackOnly()
    }

    assert(0 === callCount)
  }


  dbTest("Callbacks are not invoked on rolled back commit (exception)")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ) { conn ?=>
    var callCount = 0

    try
      conn.allOrNothing { tx ?=>
        sql"""
          INSERT INTO test VALUES(1, 'Hello')
        """.update()

        tx.onCommit(() => callCount += 1)
        tx.setRollbackOnly()
      }
      fail("Exception expected")
    catch
      case _: Throwable => ()

    assert(0 === callCount)
  }


  dbTest("Callbacks set on nested transaction are invoked on successfull commit")(
    """CREATE TABLE test(
        id INT PRIMARY KEY NOT NULL,
        value VARCHAR(445)
      )"""
  ) { conn ?=>
    var callCount = 0

    conn.allOrNothing { tx ?=>
      sql"""
        INSERT INTO test VALUES(1, 'Hello')
      """.update()

      tx.allOrNothing { sub ?=>
        sub.onCommit(() => callCount += 1)
      }
    }

    assert(1 === callCount)
  }
end TransactionTest
