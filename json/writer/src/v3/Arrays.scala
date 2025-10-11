package io.github.maxkar
package json.writer.v3

import fun.typeclass.Applicative
import fun.typeclass.Monad
import io.github.maxkar.json.writer.v3.Strings.write

/** Utilities for writing arrays. */
object Arrays {
  /** Array start character. */
  val ARRAY_START = '['

  /** Array end test. */
  val ARRAY_END = ']'

  /** Array end test. */
  val ARRAY_SEPARATOR = ','


  /** Formatter for the array layout on a given stream `S`. */
  trait Layout[M[_], -S] {
    /**
     * Puts whitespaces before value in the array.
     * @param stream stream to put value into.
     * @param first indicates if whitespaces are put before the first element or
     *   any consequent element.
     */
    def beforeValue(stream: S, first: Boolean): M[Unit]

    /** Puts whitespaces before element separator. */
    def beforeElementSeparator(stream: S): M[Unit]

    /**
     * Puts whitespaces before array end.
     * @param stream stream to put whitespacse into.
     * @param isEmpty indicates if the array is empty (no elements written) or not.
     */
    def beforeArrayEnd(stream: S, isEmpty: Boolean): M[Unit]
  }


  object Layout {
    /** Compact layout - no whitespaces. */
    final class Compact[M[_]: Applicative] extends Layout[M, Any] {
      private val pass = Applicative.pure(())
      override def beforeValue(stream: Any, first: Boolean): M[Unit] = pass
      override def beforeElementSeparator(stream: Any): M[Unit] = pass
      override def beforeArrayEnd(stream: Any, isEmpty: Boolean): M[Unit] = pass
    }


    /** Indented array formatter. */
    final class Indent[M[_]: Monad, S: DefaultWriter.In[M]] private(
          generalIndent: CharSequence,
          lastIndent: CharSequence
        ) extends Layout[M, S] {
      private val pass = Monad.pure(())

      override def beforeValue(stream: S, first: Boolean): M[Unit] =
        stream.write('\n') >=|| stream.write(generalIndent)

      override def beforeElementSeparator(stream: S): M[Unit] = pass

      override def beforeArrayEnd(stream: S, isEmpty: Boolean): M[Unit] = {
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


  /** One-by-one writer for array elements `J`. */
  final class Writer[M[_]: Monad, -S: CharWriter.In[M], J](
        stream: S,
        layout: Layout[M, S],
        writeValue: (S, J) => M[Unit]
      ) {
    /** We are before the first element in the array. */
    private var beforeFirst = true


    /** Outputs next element of the array. */
    def write(element: J): M[Unit] = {
      if beforeFirst then {
        beforeFirst = false
        layout.beforeValue(stream, true) >=|| writeValue(stream, element)
      } else {
        layout.beforeElementSeparator(stream) >=||
          stream.write(ARRAY_SEPARATOR) >=||
          layout.beforeValue(stream, false) >=||
          writeValue(stream, element)
      }
    }


    /** Finishes writing the array. */
    def finish(): M[Unit] =
      layout.beforeArrayEnd(stream, beforeFirst) >=|| stream.write(ARRAY_END)
  }


  /** Starts writing the array and creates "element-by-element" writer API. */
  def startWriting[M[_]: Monad, S: CharWriter.In[M], J](
        stream: S,
        layout: Layout[M, S],
        writeValue: (S, J) => M[Unit]
      ): M[Writer[M, S, J]] =
    stream.write(ARRAY_START) >-| { new Writer(stream, layout, writeValue) }


  /**
   * Writes all the elements of the array in the given layout using the provided
   * per-element function.
   */
  def writeAll[M[_]: Monad, S: CharWriter.In[M], J](
        stream: S,
        layout: Layout[M, S],
        writeValue: (S, J) => M[Unit],
        elements: Iterable[J]
      ): M[Unit] =
    startWriting(stream, layout, writeValue) >=>> { writer =>
      val itr = elements.iterator

      def writeNext(): M[Unit] =
        if itr.hasNext then
          writer.write(itr.next()) >=|| writeNext()
        else
          writer.finish()

      writeNext()
    }
}
