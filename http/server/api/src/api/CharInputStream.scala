package io.github.maxkar
package http.server.api

import java.nio.CharBuffer

/** Reader of the "stream of characters". The read operations may happen asynchronously. */
trait CharInputStream[M[_]]:
  /**
   * Reads the next portion of the input as the (server-managed) byte buffer.
   *
   * The buffer is set-up for the read operation. `position` points to the start
   * of the received data and `limit` to the end of the received (unprocessed) data.
   * The client must read (i.e. advance) the pointer before calling the `read` next time.
   * It may leave **unconsumed** portion that will be available on the next operation.
   *
   * @param minSize a minimal advised amount of bytes to have in the buffer before returning
   *   from the `read` operation unless end-of-stream is reached. The call may still return
   *   less that `minSize` if end of  stream was reached. For the efficient work the value
   *   should be relatively low (i.e. in the range 5-25 bytes).
   * @return buffer with the data read. The buffer may have `remaining() <= minSize` when end
   *   of the stream was reached. In particular, the returned buffer will have `remaining() == 0`
   *   if consumer always consumes all the data returned.
   */
  def read(minSize: Int): M[CharBuffer]
end CharInputStream
