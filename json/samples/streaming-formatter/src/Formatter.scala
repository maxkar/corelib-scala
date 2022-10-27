package io.github.maxkar
package json.sample.formatter.streaming

import fun.coroutine.Coroutine

import java.io.Reader
import java.io.Writer
import java.nio.CharBuffer

import text.input.BufferLookAheadStream
import text.input.BufferLookAheadStream.Filler
import text.input.LookAheadStream
import text.input.LocationLookAheadStream
import text.output.{Stream => OutStream}

import json.parser.Errors

import io.github.maxkar.json.parser.Errors.SimpleHandler
import io.github.maxkar.fun.coroutine.Coroutine.RunResult

object Formatter:
  private val module = new Coroutine[Suspension]
  import module._
  import module.given

  /** How the execution could be suspenden. */
  enum Suspension[T]:
    case Input(
      reader: Reader,
      buffer: CharBuffer,
      minSize: Int,
      continue: Unit => Coroutine.RunResult[Suspension, T]
    )

    case Output(
      writer: Writer,
      data: CharSequence,
      continue: Unit => Coroutine.RunResult[Suspension, T]
    )

    case Failure(message: String)
  end Suspension


  /** Implementation of the reader. */
  private final class ReaderFiller(reader: Reader) extends Filler[Routine]:
    override def fill(buffer: CharBuffer, minCharsToRead: Int): Routine[Unit] =
      module.suspend(new Suspender[Unit] {
        override def encode[V](continue: Unit => Coroutine.RunResult[Suspension, V]): Suspension[V] =
          Suspension.Input(reader, buffer, minCharsToRead, continue)
      })
  end ReaderFiller


  /** Implementation of the writer. */
  private final class WriterOutStream(writer: Writer) extends OutStream[Routine]:
    override def write(data: CharSequence): Routine[Unit] =
      module.suspend(new Suspender[Unit] {
        override def encode[V](continue: Unit => Coroutine.RunResult[Suspension, V]): Suspension[V] =
          Suspension.Output(writer, data, continue)
      })
  end WriterOutStream


  /** Implementation of simple errors. */
  private object RoutineRaise extends Errors.SimpleHandler[Routine, LocationLookAheadStream[Routine, Any]]:
    override def raise[T](stream: LocationLookAheadStream[Routine, Any], message: String): Routine[T] =
      module.suspend(new Suspender[T] {
        override def encode[V](continue: T => Coroutine.RunResult[Suspension, V]): Suspension[V] =
          val loc = stream.location
          Suspension.Failure(s"(${loc.line}:${loc.column}): ${message}")
      })
    end raise
  end RoutineRaise

  /** Errors implementation. */
  private val errs = Errors.simple(RoutineRaise)


  /** Runs the computation and returns either error string or none if everything was copied. */
  private def run(from: Reader, to: Writer, indent: Indent[Routine]): Option[String] =
    val filler = new ReaderFiller(from)
    val bufferedInput = BufferLookAheadStream(filler, CharBuffer.allocate(2048))
    val locationStream = LocationLookAheadStream(bufferedInput)

    val output = new WriterOutStream(to)

    val fmt = new JsonFormatter(locationStream, output, indent, errs, errs.endOfFileErrors)
    runRoutine(fmt.copy())
  end run


  /** Compacts JSON from `form` into `to` returing optional error message. */
  def compact(from: Reader, to: Writer): Option[String] =
    run(from, to, new NoIndent)


  /** Pretty-prints JSON from `form` into `to` returing optional error message. */
  def prettify(from: Reader, to: Writer): Option[String] =
    run(from, to, new RegularIndent)


  /** Runs the coroutine. */
  private def runRoutine(rt: Routine[_]): Option[String] =
    var result = Coroutine.run(rt)
    while true do
      result match
        case RunResult.Suspended(Suspension.Input(reader, buf, sz, cont)) =>
          var remaining = sz
          while remaining > 0 do
            val rd = reader.read(buf)
            if rd < 0 then
              remaining = -1
            else
              remaining -= rd
          end while
          result = cont(())
        case RunResult.Suspended(Suspension.Output(writer, data, cont)) =>
          writer.append(data)
          result = cont(())
        case RunResult.Suspended(Suspension.Failure(message)) =>
          return Some(message)
        case RunResult.Finished(result) =>
          return None
      end match
    end while
    return None
  end runRoutine
end Formatter
