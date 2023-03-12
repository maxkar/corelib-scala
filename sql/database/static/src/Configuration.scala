package io.github.maxkar
package sql.database.static

import backoff.strategy.BackoffStrategy

import java.util.concurrent.ThreadFactory

/**
 * Configuration of the database access - ways to access the database and how/when
 * to check the connection.
 *
 * @param connection connection configuration - what is the database address.
 * @param poolSize number of connections to have and maintain.
 * @param threadFactory factory to use for database access threads. Uses a thread
 *   factory that creates daemon threads by default.
 * @param backoffStrategy factory for creating back-off strategies for individual
 *   threads and connections.
 * @param validation connection validation configuration.
 */
case class Configuration(
      connection: Configuration.Connection,
      poolSize: Int = 1,
      threadFactory: ThreadFactory = Configuration.defaultThreadFactory,
      backoffStrategy: BackoffStrategy = Configuration.defaultBackoffStrategy,
      validation: Configuration.Validation = Configuration.defaultValidation,
    )


object Configuration:
  /** How to connect to the database. */
  enum Connection:
    /** Simple URL but no login/password. */
    case NoCredentials(url: String)
    /** Login and password are available for connection. */
    case LoginPassword(url: String, login: String, password: String)
    /** Property-based configuration. */
    case PropertyBased(url: String, properties: java.util.Properties)
  end Connection


  /**
   * Connection validation options.
   * @param queryTimeoutMs timeout to use on the validation query.
   * @param validateOnIdleMsTimeout timeout to use for validating an "idle"
   *   connection. Set to 0 or negative value to avoid validation.
   * @param validateUrl if the URL (connection) should be validated on the service
   *   start-up (not inside the worker). The service won't start if the `validateUrl`
   *   is set and connection could not be established. Otherwise the service will
   *   start but connections would be unavailable for tasks.
   */
  case class Validation(
      validationTimeoutMs: Int = 5000,
      validateOnIdleMsTimeout: Int = 30000,
      validateUrl: Boolean = true,
  )


  /** Default connection validation parameters. */
  val defaultValidation: Validation = Validation()


  /** Default thread factory implementation. */
  private lazy val defaultThreadFactory: ThreadFactory =
    new ThreadFactory {
      /** Counter of the threads. */
      private val threadCounter = new java.util.concurrent.atomic.AtomicInteger()

      override def newThread(x: Runnable): Thread =
        val res = new Thread(x)
        res.setDaemon(true)
        res.setName(s"Database access thread ${threadCounter.incrementAndGet()}")
        res
      end newThread
    }


  /** Default backoff strategy (randomized doubling of timeout). */
  private lazy val defaultBackoffStrategy: BackoffStrategy =
    BackoffStrategy.randomizedDouble(25, 60_000)


end Configuration
