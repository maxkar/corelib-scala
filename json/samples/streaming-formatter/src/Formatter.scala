package io.github.maxkar
package json.sample.formatter.streaming

import fun.coroutine.Coroutine
import fun.coroutine.Coroutine.RunResult

import java.io.Reader
import java.io.Writer
import java.nio.CharBuffer

import text.input.BufferLookAheadStream
import text.input.BufferLookAheadStream.Filler
import text.input.LookAheadStream
import text.input.LocationLookAheadStream
import text.output.{Stream => OutStream}

import json.parser.Errors
import json.parser.Errors.SimpleHandler

object Formatter {
  private val module = new Coroutine[Suspension]
  import module._
  import module.given

  /** How the execution could be suspenden. */
  enum Suspension[T] {
    case Input(
      reader: Reader,
      buffer: CharBuffer,
      minSize: Int,
    ) extends Suspension[Unit]

    case Output(
      writer: Writer,
      data: CharSequence,
    ) extends Suspension[Unit]

    case Failure(message: String)
  }


  /** Implementation of the reader. */
  private final class ReaderFiller(reader: Reader) extends Filler[Routine] {
    override def fill(buffer: CharBuffer, minCharsToRead: Int): Routine[Unit] =
      module.suspend(Suspension.Input(reader, buffer, minCharsToRead))
  }


  /** Implementation of the writer. */
  private final class WriterOutStream(writer: Writer) extends OutStream[Routine] {
    override def write(data: CharSequence): Routine[Unit] =
      module.suspend(Suspension.Output(writer, data))
  }


  /** Implementation of simple errors. */
  private object RoutineRaise extends Errors.SimpleHandler[Routine, LocationLookAheadStream[Routine, Any]] {
    override def raise[T](stream: LocationLookAheadStream[Routine, Any], message: String): Routine[T] =
      val loc = stream.location
      module.suspend(
        Suspension.Failure(s"(${loc.line}:${loc.column}): ${message}")
      )
  }

  /** Errors implementation. */
  private val errs = Errors.simple(RoutineRaise)


  /** Runs the computation and returns either error string or none if everything was copied. */
  private def run(from: Reader, to: Writer, indent: Indent[Routine]): Option[String] = {
    val filler = new ReaderFiller(from)
    val bufferedInput = BufferLookAheadStream(filler, CharBuffer.allocate(2048))
    val locationStream = LocationLookAheadStream(bufferedInput)

    val output = new WriterOutStream(to)

    val fmt = new JsonFormatter(locationStream, output, indent, errs, errs.endOfFileErrors)
    runRoutine(fmt.copy())
  }


  /** Compacts JSON from `form` into `to` returing optional error message. */
  def compact(from: Reader, to: Writer): Option[String] =
    run(from, to, new NoIndent)


  /** Pretty-prints JSON from `form` into `to` returing optional error message. */
  def prettify(from: Reader, to: Writer): Option[String] =
    run(from, to, new RegularIndent)


  /** Runs the coroutine. */
  private def runRoutine(rt: Routine[?]): Option[String] = {
    var proc = rt
    while true do {
      module.run(proc) match {
        case RunResult.Suspended(Suspension.Input(reader, buf, sz), cont) =>
          var remaining = sz
          while remaining > 0 do
            val rd = reader.read(buf)
            if rd < 0 then
              remaining = -1
            else
              remaining -= rd
          end while
          proc = cont(())
        case RunResult.Suspended(Suspension.Output(writer, data), cont) =>
          writer.append(data)
          proc = cont(())
        case RunResult.Suspended(Suspension.Failure(message), _) =>
          return Some(message)
        case RunResult.Finished(result) =>
          return None
      }
    }
    return None
  }
}
