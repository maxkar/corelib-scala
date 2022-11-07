package io.github.maxkar
package http.server.toolkit

import fun.typeclass.Monad

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction

import http.server.api.ByteBufferReader
import http.server.api.CharBufferReader


/**
 * A character reader that performs character decoding.
 * @param inputBuffer buffer to read from.
 * @param decoder used decoder instance.
 * @param peer peer data reader.
 * @param badChar callback to invoke when bad character is encountered.
 */
final class DecoderCharBufferReader[M[_]: Monad](
      inputBuffer: ByteBuffer,
      decoder: CharsetDecoder,
      peer: ByteBufferReader[M],
      badChar: (ByteBuffer, Int) => M[Boolean],
    )
    extends CharBufferReader[M]:
  import DecoderCharBufferReader._

  /** Current parsing state. */
  private var state: State = State.NeedInput

  override def read(buffer: CharBuffer): M[Boolean] =
    state match
      case State.NeedInput =>
        buffer.compact()
        peer.read(inputBuffer) flatMap { hasMore =>
          tryOutput(hasMore, buffer)
        }
      case State.NeedOutput =>
        tryOutput(true, buffer)
      case State.NeedLastOutput =>
        tryOutput(false, buffer)
      case State.DoFinal =>
        doFinal(buffer)
      case State.Eof =>
        Monad.pure(false)
    end match
  end read


  /** Attempts to output data into the buffer. */
  private def tryOutput(hasMore: Boolean, out: CharBuffer): M[Boolean] =
    inputBuffer.flip()
    val start = out.position()
    val res = decoder.decode(inputBuffer, out, !hasMore)
    if res.isError() then
      return badChar(inputBuffer, res.length())
    if res.isOverflow() then
      state = if hasMore then State.NeedOutput else State.NeedLastOutput
      return Monad.pure(true)
    /* State is undeflow. */
    if hasMore then
      /* We got too few chars to encode. Need to read more before we can return. */
      state = State.NeedInput
      return peer.read(inputBuffer) flatMap { hasMore =>
        tryOutput(hasMore, out)
      }

    doFinal(out)
  end tryOutput


  /** Performs the last pass of the decoding into the given buffer. */
  private def doFinal(out: CharBuffer): M[Boolean] =
    val res = decoder.flush(out)
    /* There are two cases how we can get here (and get overflow):
     *  1. From the tryOutput by finishing the stream. Overflow is OK here,
     *     the buffer may be up to the limit thus no more data could be read.
     *  2. From the read (with a new buffer). This may be bad as user expects
     *     to write more data. Hopefully decoder _will_ fill some data in the
     *     buffer and update its state. Or the reader will provide large-enough
     *     buffer. But anyway, this may cause issue with some exotic encoding(s).
     *     Not an issue with the most common ones like UTF-8 or the other ones with
     *     less than 1 character per byte.
     */
    if res.isOverflow() then
      state = State.DoFinal
      return Monad.pure(true)

    /* Everything was read. */
    state = State.Eof
    return Monad.pure(false)
  end doFinal


end DecoderCharBufferReader


object DecoderCharBufferReader:
  /** Reader state. */
  private enum State:
    /** We need more input to parse data. */
    case NeedInput
    /** We have enough input and the codec asked for more output space. */
    case NeedOutput
    /** We have enough input and the codec asked for more output space, the bytes are last. */
    case NeedLastOutput
    /** We are going to Flush the buffer. */
    case DoFinal
    /** End of file was reached. */
    case Eof


  /**
   * Creates a new reader that uses all the existing buffers and decoders.
   */
  def apply[M[_]: Monad](
        peer: ByteBufferReader[M],
        badChar: (ByteBuffer, Int) => M[Boolean],
        decoder: CharsetDecoder,
        inputBuffer: ByteBuffer,
      ): DecoderCharBufferReader[M] =
    new DecoderCharBufferReader[M](inputBuffer, decoder, peer, badChar)


  /**
   * Creates a new reader that uses an existing buffer (it is advised to
   * have a backing array in it) and the given charset.
   */
  def apply[M[_]: Monad](
        peer: ByteBufferReader[M],
        badChar: (ByteBuffer, Int) => M[Boolean],
        charset: Charset,
        inputBuffer: ByteBuffer,
      ): DecoderCharBufferReader[M] =
    val decoder =
      charset.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
    DecoderCharBufferReader(peer, badChar, decoder, inputBuffer)
  end apply


  /**
   * Creates a new reader that uses a fresh new buffer.
   */
  def apply[M[_]: Monad](
        peer: ByteBufferReader[M],
        badChar: (ByteBuffer, Int) => M[Boolean],
        charset: Charset,
        bufferSize: Int = 2048,
      ): DecoderCharBufferReader[M] =
    val buffer = ByteBuffer.allocate(bufferSize)
    DecoderCharBufferReader(peer, badChar, charset, buffer)
  end apply
end DecoderCharBufferReader
