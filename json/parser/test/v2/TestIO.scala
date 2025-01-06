package io.github.maxkar
package json.parser.v2

import text.v2.input.Reader
import text.v2.input.BufferedLookAhead

import fun.typeclass.Monad
import fun.instances.Unnest

import java.io.IOException
import java.io.{Reader => JReader}

import org.scalatest.Assertions
import io.github.maxkar.json.parser.Whitespaces.parse


/** Common definitions for JSON test input/output. */
object TestIO {
  export Unnest.given

  /** Type of the test operation. */
  type Operation = Unnest


  /** Type of the stream used in tests. */
  type IOStream = BufferedLookAhead[java.io.Reader]


  /** Error in parsing the stream. Captures offset and message. */
  case class ParseException(offset: Int, message: String) extends IOException(s"${offset}: message")


  /** Raises an error. */
  private def raiseImpl[T](context: IOStream, message: String): Unnest[T] =
    context.getLocation() <| { loc =>
      throw new ParseException(loc.offset, message)
    }


  /** Raise function for use with "raise" factory methods for error handlers. */
  val raise: [T] => (IOStream, String) => Operation[T] = [T] => (ctx, err) => raiseImpl(ctx, err)



  /** Opens IO stream for the string. */
  def stringInput(data: String): IOStream =
    BufferedLookAhead(new java.io.StringReader(data), 100)


  /**
   * Runs the parses and expect the common parsing error. Returns the
   * parsing error or fails an assertion.
   */
  def parseWithError(block: => Any): ParseException =
    Assertions.intercept[ParseException] { block }


  /** Runs IO operation and returns the result. */
  def doIO[T](io: Operation[T]): T =
    Unnest.run(io)


  /** Errors for unnest buffered IO. */
  given unnestError: BufferedLookAhead.IOErrors[Unnest, java.io.Reader] =
    BufferedLookAhead.IOErrors.raise(raise)


  /** Reader for java instances. */
  given javaReaderReader[M[_]: Monad]: Reader[M, JReader] with {
    override def read(source: JReader, target: Array[Char], targetStart: Int, targetEnd: Int): M[Int] =
      Monad.pure(source.read(target, targetStart, targetEnd - targetStart))
  }
}
