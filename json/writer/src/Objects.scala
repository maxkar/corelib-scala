package io.github.maxkar
package json.writer

import text.output.Stream

import fun.typeclass.Monad
import fun.typeclass.Applicative

/** Object output utilities. */
object Objects:
  /** Object start sequence. */
  val OBJECT_START = "{"

  /** Object end sequence. */
  val OBJECT_END = "}"

  /** Object key-value separator. */
  val KEY_VALUE_SEPARATOR = ":"

  /** Object elements (key-value pair) separator. */
  val ELEMENT_SEPARATOR = ","

  /**
   * Whitespace writer for objects. Methods of this whitespacer are invoked to
   * format the output being generated.
   */
  trait Whitespaces[M[_], S]:
    /** Outputs whitespaces before the object. */
    def beforeObject(stream: S): M[Unit]

    /** Outputs whitespaces after the object. */
    def afterObject(stream: S): M[Unit]

    /** Outputs whitespaces that should be present inside the empty object. */
    def insideEmptyObject(stream: S): M[Unit]

    /** Outputs whitespaces before first object key. */
    def beforeFirstKey(stream: S): M[Unit]

    /** Outputs whitespaces before an object key (whis is not the first key-value pair). */
    def beforeKey(stream: S): M[Unit]

    /** Outputs whitespaces after object key. */
    def afterKey(stream: S): M[Unit]

    /** Outputs whitespaces that should be present before value associated with the previous key. */
    def beforeValue(stream: S): M[Unit]

    /** Outputs whitespacse after the last value (before closing the object). */
    def afterLastValue(stream: S): M[Unit]

    /** Outputs whitespaces after (non-last) object value. */
    def afterValue(stream: S): M[Unit]
  end Whitespaces


  object Whitespaces:
    /** Creates a whitespace placement strategy that puts no whitespaces. */
    def nothing[M[_]: Applicative, Any]: Whitespaces[M, Any] =
      new Whitespaces[M, Any]:
        /** Universal return value. */
        private val pass = Monad.pure(())

        override def beforeObject(stream: Any): M[Unit] = pass
        override def afterObject(stream: Any): M[Unit] = pass
        override def insideEmptyObject(stream: Any): M[Unit] = pass
        override def beforeFirstKey(stream: Any): M[Unit] = pass
        override def beforeKey(stream: Any): M[Unit] = pass
        override def afterKey(stream: Any): M[Unit] = pass
        override def beforeValue(stream: Any): M[Unit] = pass
        override def afterLastValue(stream: Any): M[Unit] = pass
        override def afterValue(stream: Any): M[Unit] = pass
      end new
    end nothing
  end Whitespaces


  /**
   * (Stateful) object layout writer - writes whitespaces, boundaries and separators
   * but does not write keys or values by themselves.
   */
  final class Layout[M[_]: Monad, S <: Stream[M]] private[Objects](
        whitespaces: Whitespaces[M, S]
      ):
    /** If any values were observed. */
    private var seenValue = false


    /**
     * Prepares the stream for the next key-value pair by writing appropriate
     * whitespaces and entity separators.
     */
    def prepareKey(stream: S): M[Unit] =
      if seenValue then
        prepareNext(stream)
      else
        prepareFirst(stream)
    end prepareKey


    /** Prepares the stream for writing value (after key was written). */
    def prepareValue(stream: S): M[Unit] =
      for
        _ <- whitespaces.afterKey(stream)
        _ <- writeKeyValueSeparator(stream)
        res <- whitespaces.beforeValue(stream)
      yield res


    /** Finishes the stream by writing appropriate whitespaces and object terminator. */
    def end(stream: S): M[Unit] =
      val base =
        if seenValue then
          whitespaces.afterLastValue(stream)
        else
          whitespaces.insideEmptyObject(stream)

      for
        _ <- base
        _ <- writeObjectEnd(stream)
        res <- whitespaces.afterObject(stream)
      yield res
    end end


    /** Prepares for the "next" (non-first) value. */
    private def prepareNext(stream: S): M[Unit] =
      for
        _ <- whitespaces.afterValue(stream)
        _ <- writeElementSeparator(stream)
        res <- whitespaces.beforeKey(stream)
      yield res
    end prepareNext


    /** Prepares for the first value in the stream. */
    private def prepareFirst(stream: S): M[Unit] =
      seenValue = true
      whitespaces.beforeFirstKey(stream)
    end prepareFirst
  end Layout


  /**
   * Convenient "push-style" writer.
   * @tparam M type of IO monad.
   * @tparam T type of the element being written.
   */
  abstract sealed class Writer[M[_], -T]:
    /** Outputs next key-value pair. */
    def entry(key: String, v: T): M[Unit]

    /** Finishes writing and closes the array output. */
    def end(): M[Unit]
  end Writer


  /** Array writer implementation (hides some types). */
  private final class WriterImpl[M[_]: Monad, S <: Stream[M], T](
        layout: Layout[M, S],
        writeElement: (T, S) => M[Unit],
        stream: S
      ) extends Writer[M, T]:

    override def entry(key: String, v: T): M[Unit] =
      for
        _ <- layout.prepareKey(stream)
        _ <- Strings.writeString(key, stream)
        _ <- layout.prepareValue(stream)
        res <- writeElement(v, stream)
      yield res
    end entry

    override def end(): M[Unit] =
      layout.end(stream)
  end WriterImpl


  /** Writes object start sequence (prologue). */
  def writeObjectStart[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(OBJECT_START)


  /** Writes object end sequence (epilogue). */
  def writeObjectEnd[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(OBJECT_END)


  /** Writes key-value separator. */
  def writeKeyValueSeparator[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(KEY_VALUE_SEPARATOR)


  /** Writes element (key-value-pair) separator. */
  def writeElementSeparator[M[_]](stream: Stream[M]): M[Unit] =
    stream.write(ELEMENT_SEPARATOR)


  /**
   * Starts laying out an object in the output stream according to
   * the whitespace rules. The method writes starting whitespaces and
   * array prologue and then returns a configured `Layout` instance.
   * The implementation should then call `Layout.prepareKey(_)` before
   * each key-value pair, `Layout.prepareValue(_)` befor value and
   * `Layout.end()` after the last entry.
   *
   * The pseudocode is as follows:
   * ```
   * val layout = Objects.beginObject(whitespaces, stream)
   * while hasNextElement do
   *   layout.prepareKey(stream)
   *   writeNextKey
   *   layout.prepareValue(stream)
   *   writeNextValue
   * layout.end(stream)
   * ```
   *
   * @param whitespaces whitespace rules.
   */
  def beginObject[M[_]: Monad, S <: Stream[M]](
        whitespaces: Whitespaces[M, S],
        stream: S
      ): M[Layout[M, S]] =
    for
      _ <- whitespaces.beforeObject(stream)
      _ <- writeObjectStart(stream)
    yield
      new Layout(whitespaces)
  end beginObject


  /**
   * Creates a new "push" protocol that writes data into the
   * provided stream and uses the given strategy to put whitespaces.
   *
   * The pseudocode is as follows:
   * ```
   * val writer = Objects.newWriter(whitespaces, stream)
   * while hasNextEntry
   *   val (key, value) = nextEntry
   *   writer.entry(key, value)
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
    beginObject(whitespaces, stream).map { layout =>
      new WriterImpl(layout, writeElement, stream)
    }
  end newWriter


  /** Writes the `data` as an object. */
  def writeAll[M[_]: Monad, S <: Stream[M], T](
        whitespaces: Whitespaces[M, S],
        writeElement: (T, S) => M[Unit],
        data: Iterator[(String, T)],
        stream: S,
      ): M[Unit] =
    newWriter(whitespaces, writeElement, stream).flatMap { w =>
      writeAllNext(data, w)
    }

  /** Pass rest of the data (one by one) to the writer. */
  private def writeAllNext[M[_]: Monad, T](
        data: Iterator[(String, T)],
        writer: Writer[M, T],
      ): M[Unit] =
    if data.hasNext then
      val (key, value) = data.next()
      writer.entry(key, value).flatMap { _ => writeAllNext(data, writer) }
    else
      writer.end()
  end writeAllNext
end Objects
