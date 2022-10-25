package io.github.maxkar
package json.writer

import text.output.Stream

import fun.typeclass.Monad
import fun.typeclass.Applicative


/** Array output utilities. */
object Arrays:
  /** Array start sequence. */
  val ARRAY_START = "["

  /** Array end sequence. */
  val ARRAY_END = "]"

  /** Array element separator. */
  val ARRAY_SEPARATOR = ","


  /**
   * Whitespace writer for arrays. Methods of this whitespacer are invoked to
   * format the output being generated.
   */
  trait Whitespaces[M[_], -S]:
    /** Outputs whitespaces before the array. */
    def beforeArray(stream: S): M[Unit]

    /** Outputs whitespaces after the array. */
    def afterArray(stream: S): M[Unit]

    /** Outputs whitespaces that should be present inside the empty array. */
    def insideEmptyArray(stream: S): M[Unit]

    /** Outputs whitespaces before first array element. */
    def beforeFirstValue(stream: S): M[Unit]

    /** Outputs whitespaces before an array element (whis is not the first element). */
    def beforeValue(stream: S): M[Unit]

    /** Outputs whitespacse after the last value (before closing the array). */
    def afterLastValue(stream: S): M[Unit]

    /** Outputs whitespaces after (non-last) array value. */
    def afterValue(stream: S): M[Unit]
  end Whitespaces


  object Whitespaces:
    /** Creates a whitespace placement strategy that puts no whitespaces. */
    def nothing[M[_]: Applicative, Any]: Whitespaces[M, Any] =
      new Whitespaces[M, Any]:
        /** Universal return value. */
        private val pass = Monad.pure(())

        override def beforeArray(stream: Any): M[Unit] = pass
        override def afterArray(stream: Any): M[Unit] = pass
        override def insideEmptyArray(stream: Any): M[Unit] = pass
        override def beforeFirstValue(stream: Any): M[Unit] = pass
        override def beforeValue(stream: Any): M[Unit] = pass
        override def afterLastValue(stream: Any): M[Unit] = pass
        override def afterValue(stream: Any): M[Unit] = pass
      end new
    end nothing
  end Whitespaces


  /**
   * (Stateful) array layout writer - writes whitespaces, boundaries and separators
   * but does not write values by itself.
   */
  final class Layout[M[_]: Monad, S <: Stream[M]] private[Arrays](
        whitespaces: Whitespaces[M, S]
      ):
    /** If any values were observed. */
    private var seenValue = false


    /**
     * Prepares the stream for the next value by writing appropriate whitespaces
     * and value separators.
     */
    def prepareValue(stream: S): M[Unit] =
      if seenValue then
        prepareNext(stream)
      else
        prepareFirst(stream)
    end prepareValue


    /** Finishes the stream by writing appropriate whitespaces and array terminator. */
    def end(stream: S): M[Unit] =
      val base =
        if seenValue then
          whitespaces.afterLastValue(stream)
        else
          whitespaces.insideEmptyArray(stream)

      for
        _ <- base
        _ <- writeArrayEnd(stream)
        res <- whitespaces.afterArray(stream)
      yield res
    end end


    /** Prepares for the "next" (non-first) value. */
    private def prepareNext(stream: S): M[Unit] =
      for
        _ <- whitespaces.afterValue(stream)
        _ <- writeArraySeparator(stream)
        res <- whitespaces.beforeValue(stream)
      yield res
    end prepareNext


    /** Prepares for the first value in the stream. */
    private def prepareFirst(stream: S): M[Unit] =
      seenValue = true
      whitespaces.beforeFirstValue(stream)
    end prepareFirst
  end Layout


  /**
   * Convenient "push-element" writer.
   * @tparam M type of IO monad.
   * @tparam T type of the element being written.
   */
  abstract sealed class Writer[M[_], -T]:
    /** Outputs next array element. */
    def element(v: T): M[Unit]

    /** Finishes writing and closes the array output. */
    def end(): M[Unit]
  end Writer


  /** Array writer implementation (hides some types). */
  private final class WriterImpl[M[_]: Monad, S <: Stream[M], T](
        layout: Layout[M, S],
        writeElement: (T, S) => M[Unit],
        stream: S
      ) extends Writer[M, T]:

    override def element(v: T): M[Unit] =
      layout.prepareValue(stream).flatMap { _ =>
        writeElement(v, stream)
      }

    override def end(): M[Unit] =
      layout.end(stream)
  end WriterImpl


  /** Writes array start (prologue). */
  def writeArrayStart[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(ARRAY_START)


  /** Writes array end (epilogue). */
  def writeArrayEnd[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(ARRAY_END)


  /** Writes array separator. */
  def writeArraySeparator[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(ARRAY_SEPARATOR)


  /**
   * Starts laying out an array in the output stream according to
   * the whitespace rules. The method writes starting whitespaces and
   * array prologue and then returns a configured `Layout` instance.
   * The implementation should then call `Layout.prepareValue(_)` before
   * each value and `Layout.end()` at the end.
   *
   * The pseudocode is as follows:
   * ```
   * val layout = Arrays.beginArray(whitespaces, stream)
   * while hasNextElement do
   *   layout.prepareValue(stream)
   *   writeNextElement
   * layout.end(stream)
   * ```
   *
   * @param whitespaces whitespace rules.
   */
  def beginArray[M[_]: Monad, S <: Stream[M]](
        whitespaces: Whitespaces[M, S],
        stream: S
      ): M[Layout[M, S]] =
    for
      _ <- whitespaces.beforeArray(stream)
      _ <- writeArrayStart(stream)
    yield
      new Layout(whitespaces)
  end beginArray


  /**
   * Creates a new "push" protocol that writes data into the
   * provided stream and uses the given strategy to put whitespaces.
   *
   * The pseudocode is as follows:
   * ```
   * val writer = Arrays.newWriter(whitespaces, stream)
   * while hasNextElement do
   *   writer.element(nextElement)
   * writer.end()
   * ```
   *
   * @param whitespaces whitespace rules.
   * @param writeElement function to write (contents) of the element.
   */
  def newWriter[M[_]: Monad, S <: Stream[M], T](
        whitespaces: Whitespaces[M, S],
        writeElement: (T, S) => M[Unit],
        stream: S,
      ): M[Writer[M, T]] =
    beginArray(whitespaces, stream).map { layout =>
      new WriterImpl(layout, writeElement, stream)
    }
  end newWriter


  /** Writes the `data` as an array. */
  def writeAll[M[_]: Monad, S <: Stream[M], T](
        whitespaces: Whitespaces[M, S],
        writeElement: (T, S) => M[Unit],
        data: Iterator[T],
        stream: S,
      ): M[Unit] =
    newWriter(whitespaces, writeElement, stream).flatMap { w =>
      writeAllNext(data, w)
    }


  /** Pass rest of the data (one by one) to the writer. */
  private def writeAllNext[M[_]: Monad, T](
        data: Iterator[T],
        writer: Writer[M, T],
      ): M[Unit] =
    if data.hasNext then
      writer.element(data.next()).flatMap { _ => writeAllNext(data, writer) }
    else
      writer.end()
  end writeAllNext
end Arrays
