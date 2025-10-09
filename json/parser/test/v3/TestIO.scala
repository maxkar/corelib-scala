package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad
import fun.instances.Unnest

import java.io.StringReader
import java.io.IOException

import org.scalatest.Assertions

object TestIO {
  export Unnest.given
  export BufferedJsonReader.given

  /** Type of the test operation. */
  type Operation[T] = Unnest[T]

  type JsonStream = BufferedJsonReader[Operation]

  case class JsonException(offset: Int, message: String) extends IOException(message)

  /** Parses the input using the given parser and returns output and offset. */
  def parse[T](input: String, cb: JsonStream => Operation[T]): (T, Int) = {
    val stream = BufferedJsonReader(new StringReader(input))
    Unnest.run(
      for
        result <- cb(stream)
        location <- stream.getLocation()
      yield
        (result, location.offset)
    )
  }


  def run[T](input: String, cb: JsonStream => Operation[T]): T = {
    val stream = BufferedJsonReader(new StringReader(input))
    Unnest.run(cb(stream))
  }


  def raise[T](stream: BufferedJsonReader[Unnest], message: String): Operation[T] =
    stream.getLocation()  >-> { loc =>
      throw new JsonException(loc.offset, message)
    }


  def failParse(input: String, cb: JsonStream => Operation[?]): JsonException =
    Assertions.intercept[JsonException] { run(input, cb) }


  given Reader[Operation, StringReader] with {
    extension (stream: StringReader) {
      override def read(into: Array[Char], offset: Int, length: Int): Unnest[Int] =
        Monad.pure(stream.read(into, offset, length))
    }
  }

  given ParseError[Operation, JsonStream] with {
    extension (stream: JsonStream) {
      override def parseError[T](message: String): Unnest[T] =
        raise(stream, message)
    }
  }
}
