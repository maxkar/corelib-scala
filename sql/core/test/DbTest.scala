package io.github.maxkar
package sql

import connection.AutocommitConnection

/**
 * Base test for all database tests. Provides convenience test definition utilities.
 */
abstract class DbTest extends org.scalatest.funsuite.AnyFunSuite:
  /** Runs a specific test on the configured database. */
  final def dbTest(name: String)(init: String*)(cb: AutocommitConnection ?=> Unit): Unit =
    test(name) {
      TempDb.withTestDb(init*) { conn =>
        cb(using new AutocommitConnection(conn))
      }
    }

end DbTest
