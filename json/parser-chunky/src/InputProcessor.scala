package io.github.maxkar
package json.parser.chunky

import fun.typeclass.Monad
import json.parser.input.ConsumerStatus

/**
 * Processor/consumer of the input stream. It tracks parsing state internally
 * and could "suspend" processing until the next portion of input is available.
 *
 * The processor should be used in the following manner:
 * {{{
 *   val processor: InputProcessor[E] = ...
 *
 *   var input = getNextInputPortion()
 *   while !input.isEof && processor.process(input.charSeq) == input.charSeq.length:
 *     input = getNextInputPortion()
 *
 *   val res = processor.end()
 * }}}
 *
 * Instances of this class are not thread-safe.
 *
 * @tparam E error type.
 * @tparam T result type.
 * @param state current state of the processor.
 * @param unexpectedEof handler for the unexpected end of file.
 * @param md implementation of the monad typeclass for the "Parser" type associated with this processor.
 */
final class InputProcessor[E, T] private[chunky](
      private var state: Operation[E, T],
      unexpectedEof: SourceLocation => E,
      md: Monad[({type P[R] = Operation[E, R]})#P]):

  /** Character offset in the (global) input. */
  private var inputOffset = 0

  /** Current line index. */
  private var currentLine = 1

  /** Current column index. */
  private var currentColumn = 1

  /** Current chunk being processed. */
  private var chunk: CharSequence = null

  /** Offset in the current data chunk. */
  private var chunkOffset = 0

  /** Input is after CR character (so next LF won't cause newline). */
  private var afterCarriageReturn = false

  /** If process method accepts input (i.e. "no more input" was not yet indicated). */
  private var acceptsInput = true

  /** If end was called. */
  private var endCalled = false


  /**
   * Handles a portion of the input. It is converted according to the
   * parsing rules. The processing stops when:
   *
   *  * All characters are consumed from the stream. Parsing is suspended
   *    and the {{{inputPortion.length}}} is returned. The input producer
   *    may provide next portion of characters.
   *  * The parser indicated an error. The return value may be less than
   *    {{{inputPortion.length}}}.
   *  * The parser indicated it does not need more data. The return value may
   *    be less than {{{inputPortion.length}}}.
   *
   * @param inputPortion a portion of input to process.
   * @return number of characters consumed. The function does not consume data when
   *   it is reached final parsing state (value or error is available) or end() method
   *   was called.
   */
  def process(inputPortion: CharSequence): Int =
    if (!acceptsInput) then
      return 0

    /* Set up the internal parser state. */
    chunk = inputPortion
    chunkOffset = 0

    /* The "step" function may reject input. It may also advance the input
     * to some degree (up to the end of the current chunk). */
    while acceptsInput && chunkOffset < chunk.length do
      doOneReduction()

    /* Clean-up the input and allow GC to collect the buffer if needed. */
    chunk = null

    /* Return the number of consumed characters. */
    chunkOffset
  end process


  /** Finishes the processing (handles "end of input condition") and returns processing result. */
  def end(): Either[E, T] =
    while true do
      state match
        case Operation.Error(e) =>
          return Left(e)
        case Operation.FlatMap(base, fn) =>
          endBasicOperation(base) match
            case Left(e) =>
              return Left(e)
            case Right(v) =>
              state = fn(v)
          end match
        case other: Operation.BasicOperation[E, T] =>
          return endBasicOperation(other)
      end match
    throw new Error("Unreacheable code reached")
  end end


  /** Processes one "reduction" of the monadic formula. */
  private def doOneReduction(): Unit =
    state match
      case Operation.Error(_) =>
        /* Just stop processing things, we are done. */
        acceptsInput = false

      case Operation.FlatMap(base, fn) =>
        /* Flat map - try to calculate head. */
        doBasicOperation(base) match
          case Left(a) =>
            /* Head is _either_ not complete or resolved into another operation (dispatch
             * based on the next input char may do that). Rewrite using monad laws to "basic"
             * head and maybe complex tail. The rewrite should be a simple at-most-one rewrite
             * (there could be at most one "flatMap" at the start of the base).
             */
            state = md.bind(a, fn)
          case Right(v) =>
            /* Now we should be calculating Right-hand side of the flat map. */
            state = fn(v)
        end match

      case other: Operation.BasicOperation[E, T] =>
        doBasicOperation(other) match
          case Left(newState) =>
            /* The operation is not complete. Either we need more input or the processing
             * switched to another state (dispatch based on the character).
             */
            state = newState
          case Right(v) =>
            /* The simple step produced the value. And we know that is is the top-level
             * one. Set the state to pure (indicate to the end call that everything is OK)
             * and mark that we don't need more input. */
            state = Operation.Pure(v)
            acceptsInput = false
        end match
    end match
  end doOneReduction


  /**
   * Attempts to process and complet one "basic" (non-error, non-flat-map) operation by
   * feeding the current input to it. Updates input positions (both in-stream and in-chunk).
   *
   * @param operation operation to compute.
   */
  private def doBasicOperation[R](operation: Operation.BasicOperation[E, R]): Either[Operation[E, R], R] =
    operation match
      case Operation.Pure(v) =>
        Right(v)

      case Operation.StatefulScan(state, stepper) =>
        stepper(chunk.subSequence(chunkOffset, chunk.length), state) match
          case (ConsumerStatus.NeedMoreInput, newState) =>
            /* The consumer may eat more. Move position(s) and set the state as
             * "still scanning" with new consumption state.
             */
            moveInLine(chunk.length - chunkOffset)
            Left(Operation.StatefulScan(newState, stepper))
          case (ConsumerStatus.Finished(moveSize), endState) =>
            moveInLine(moveSize)
            Right(endState)
        end match

      case Operation.LookAhead(dispatchFn) =>
        Left(dispatchFn(chunk.charAt(chunkOffset)))

      case Operation.SkipChar =>
        moveInLine(1)
        Right(())

      case Operation.Location =>
        Right(currentLocation())

      case Operation.SkipWhitespaces =>
        while chunkOffset < chunk.length do
          chunk.charAt(chunkOffset) match
            case ' ' | '\t' =>
              moveInLine(1)
            case '\r' =>
              moveToNextLine()
              afterCarriageReturn = true
            case '\n' =>
              if afterCarriageReturn then
                /* We skip through both input and chunk. But it is the second half of
                 * CR-LF pair so we don't move "row/column" pointer here.
                 */
                inputOffset += 1
                chunkOffset += 1
                afterCarriageReturn = false
              else
                moveToNextLine()
            case _ => return Right(())
          end match
        end while

        Left(Operation.SkipWhitespaces)
    end match
  end doBasicOperation


  /** Ends a basic operation that does not cause other processing. */
  private def endBasicOperation[T](op: Operation.BasicOperation[E, T]): Either[E, T] =
    op match
      case Operation.Pure(r) =>
        return Right(r)
      case Operation.StatefulScan(r, _) =>
        return Right(r)
      case Operation.LookAhead(_) | Operation.SkipChar =>
        return Left(unexpectedEof(currentLocation()))
      case Operation.SkipWhitespaces =>
        Right(())
      case Operation.Location =>
        return Right(currentLocation())
  end endBasicOperation


  /** Moves the position "in a single line" (i.e. no linefeed characters). */
  private def moveInLine(size: Int): Unit =
    inputOffset += size
    chunkOffset += size
    currentColumn += size
    /* Sanity as we may get here from some stateful scan that rejected
     * all the characters including the initial one. */
    if (size > 0)
      afterCarriageReturn = false
  end moveInLine


  /** Moves the pointer to a new line. */
  private def moveToNextLine(): Unit =
    inputOffset += 1
    chunkOffset += 1
    currentLine += 1
    currentColumn = 1
  end moveToNextLine


  /** Returns current location in the file. */
  private def currentLocation(): SourceLocation =
    SourceLocation(inputOffset, currentLine, currentColumn)

end InputProcessor
