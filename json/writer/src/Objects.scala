package io.github.maxkar
package json.writer

import fun.typeclass.Applicative
import fun.typeclass.Monad

object Objects {
  /** Object start character. */
  val OBJECT_START = '{'

  /** Object end character. */
  val OBJECT_END = '}'

  /** Entry separator in the object. */
  val OBJECT_ENTRY_SEPARATOR = ','

  /** Separator between key and value. */
  val OBJECT_KEY_VALUE_SEPARATOR = ':'


  /** Formatter for the Object layout for the given stream `S`. */
  trait Layout[M[_], -S] {
    /**
     * Puts whitespaces before key in the object.
     * @param stream stream to put value into.
     * @param first indicates if whitespaces are put before the first key or
     *   any consequent key.
     */
    def beforeKey(stream: S, first: Boolean): M[Unit]

    /** Puts whitespaces before key-value separator. */
    def beforeKeyValueSeparator(stream: S): M[Unit]

    /** Puts whitespaces before entry separator. */
    def beforeEntrySeparator(stream: S): M[Unit]

    /** Puts whitespaces before values. */
    def beforeValue(stream: S): M[Unit]

    /** Puts whitespaces before entry separator. */
    def beforeObjectEnd(stream: S, isEmpty: Boolean): M[Unit]
  }


  object Layout {
    /** Compact layout - no spaces. */
    final class Compact[M[_]: Applicative] extends Layout[M, Any] {
      private val pass = Applicative.pure(())

      override def beforeKey(stream: Any, first: Boolean): M[Unit] = pass
      override def beforeKeyValueSeparator(stream: Any): M[Unit] = pass
      override def beforeEntrySeparator(stream: Any): M[Unit] = pass
      override def beforeValue(stream: Any): M[Unit] = pass
      override def beforeObjectEnd(stream: Any, isEmpty: Boolean): M[Unit] = pass
    }


    /** Indented object formatter. */
    final class Indent[M[_]: Monad, S: DefaultWriter.In[M]] private(
          generalIndent: CharSequence,
          lastIndent: CharSequence
        ) extends Layout[M, S] {
      private val pass = Applicative.pure(())

      override def beforeKey(stream: S, first: Boolean): M[Unit] =
        stream.write('\n') >=|| stream.write(generalIndent)

      override def beforeKeyValueSeparator(stream: S): M[Unit] = pass

      override def beforeEntrySeparator(stream: S): M[Unit] = pass

      override def beforeValue(stream: S): M[Unit] = stream.write(' ')

      override def beforeObjectEnd(stream: S, isEmpty: Boolean): M[Unit] = {
        if isEmpty then
          pass
        else
          stream.write('\n') >=|| stream.write(lastIndent)
      }
    }


    object Indent {
      def apply[M[_]: Monad, S: DefaultWriter.In[M]](generalIndent: Int, lastIndent: Int): Layout[M, S] =
        new Indent(new Whitespaces(generalIndent), new Whitespaces(lastIndent))
    }
  }


  /** One-by-one writer for object entries `E`. */
  final class Writer[M[_]: Monad, -S: CharWriter.In[M], E] private[Objects](
        stream: S,
        layout: Layout[M, S],
        writeKey: (S, E) => M[Unit],
        writeValue: (S, E) => M[Unit]
      ) {
    /** We are before the first element in the array. */
    private var beforeFirst = true


    /** Outputs one map entry. */
    def write(entry: E): M[Unit] = {
      val prologue =
        if beforeFirst then {
          beforeFirst = false
          layout.beforeKey(stream, true)
        } else {
          layout.beforeEntrySeparator(stream) >=||
            stream.write(OBJECT_ENTRY_SEPARATOR) >=||
            layout.beforeKey(stream, false)
        }

      prologue >=||
        writeKey(stream, entry) >=||
        layout.beforeKeyValueSeparator(stream) >=||
        stream.write(OBJECT_KEY_VALUE_SEPARATOR) >=||
        layout.beforeValue(stream) >=||
        writeValue(stream, entry)
    }


    /** Finishes writing the object. */
    def finish(): M[Unit] =
      layout.beforeObjectEnd(stream, beforeFirst) >=|| stream.write(OBJECT_END)
  }


  /** Starts writing the array and creates "element-by-element" writer API. */
  def startWriting[M[_]: Monad, S: CharWriter.In[M], E](
        stream: S,
        layout: Layout[M, S],
        writeKey: (S, E) => M[Unit],
        writeValue: (S, E) => M[Unit],
      ): M[Writer[M, S, E]] =
    stream.write(OBJECT_START) >-| { new Writer(stream, layout, writeKey, writeValue) }



  /**
   * Writes all the elements of the object in the given layout using the provided
   * key- and value-output functions
   */
  def writeAll[M[_]: Monad, S: CharWriter.In[M], E](
        stream: S,
        layout: Layout[M, S],
        writeKey: (S, E) => M[Unit],
        writeValue: (S, E) => M[Unit],
        elements: Iterable[E]
      ): M[Unit] =
    startWriting(stream, layout, writeKey, writeValue) >=>> { writer =>
      val itr = elements.iterator

      def writeNext(): M[Unit] =
        if itr.hasNext then
          writer.write(itr.next()) >=|| writeNext()
        else
          writer.finish()

      writeNext()
    }
}
