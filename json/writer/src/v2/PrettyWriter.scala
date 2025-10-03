package io.github.maxkar
package json.writer.v2

import text.v2.output.Writer

import fun.typeclass.Monad

/** Implementation of the "compact output" writer. */
private final class PrettyWriter[M[_]: Monad, T, S: Writer.In[M]](
      format: PrettyPrintOptions,
      classifier: Values.ValueClassifier[T],
    )
    extends JsonWriter[M, S, T] {

  override def write(value: T, stream: S): M[Unit] =
    new PrettyWriter.InstanceWriter(format, classifier, stream).write(value)
}


private object PrettyWriter {
  /** Writer for one value instance. */
  private[writer] final class InstanceWriter[M[_]: Monad, T, S: Writer.In[M]](
        format: PrettyPrintOptions,
        classifier: Values.ValueClassifier[T],
        stream: S
      )
      extends Values.ValueCallback[T, M[Unit]] {

    /** Current indent level. */
    private var indentLevel: Int = 0

    /** Current writing context. */
    private var context: Context = Context.TopLevel

    /** Stack of contexts. */
    private var contextStack: List[Context] = Nil

    /** No-op operation. */
    private val pass = Monad.pure(())


    /** Array whitespaces. */
    private val arrayWhitespaces =
      new Arrays.Whitespaces[M, S] {
        override def beforeArray(stream: S): M[Unit] = {
          contextStack = context :: contextStack
          pass
        }

        override def afterArray(stream: S): M[Unit] = {
          contextStack = contextStack.tail
          pass
        }

        override def beforeFirstValue(stream: S): M[Unit] = {
          context = Context.InArray
          indentLevel += 1
          beforeValue(stream)
        }

        override def beforeValue(stream: S): M[Unit] =
          wrapAndIndent()

        override def afterValue(stream: S): M[Unit] = pass

        override def afterLastValue(stream: S): M[Unit] = {
          indentLevel -= 1
          wrapAndIndent()
        }

        override def insideEmptyArray(stream: S): M[Unit] =
          if context.shouldWrap(format.emptyArrayWrap) then
            wrapAndIndent()
          else
            pass
      }


    /** Object whitespaces logic. */
    val objectWhitespaces =
      new Objects.Whitespaces[M, S] {
        override def beforeObject(stream: S): M[Unit] = {
          contextStack = context :: contextStack
          pass
        }

        override def afterObject(stream: S): M[Unit] = {
          contextStack = contextStack.tail
          pass
        }

        override def beforeFirstKey(stream: S): M[Unit] = {
          indentLevel += 1
          context = Context.InObject
          beforeKey(stream)
        }

        override def beforeKey(stream: S): M[Unit] =
          wrapAndIndent()

        override def afterKey(stream: S): M[Unit] = pass

        override def beforeValue(stream: S): M[Unit] =
          stream.write(" ")

        override def afterValue(stream: S): M[Unit] = pass

        override def afterLastValue(stream: S): M[Unit] = {
          indentLevel -= 1
          wrapAndIndent()
        }

        override def insideEmptyObject(stream: S): M[Unit] =
          if context.shouldWrap(format.emptyObjectWrap) then
            wrapAndIndent()
          else
            pass
      }


    /** Writes single value into the stream. */
    def write(value: T): M[Unit] = classifier.classifyValue(value, this)


    /** Element writer, for compatibility with the iterator/object APIs. */
    def writeElt(value: T, stream: S): M[Unit] =
      classifier.classifyValue(value, this)

    override def boolean(v: Boolean): M[Unit] =
      Literals.writeBoolean(v, stream)

    override def nullValue(): M[Unit] =
      Literals.writeNull(stream)

    override def string(v: CharSequence): M[Unit] =
      Strings.writeString(v, stream)

    override def number(representation: CharSequence): M[Unit] =
      stream.write(representation)

    override def array(iter: Iterator[T]): M[Unit] =
      Arrays.writeAll(arrayWhitespaces, writeElt, iter, stream)

    override def unorderedObject(iter: Iterator[(String, T)]): M[Unit] =
      if format.sortObjectKeys then
        objectFromIter(iter.toSeq.sortBy(_._1).iterator)
      else
        objectFromIter(iter)

    override def orderedObject(iter: Iterator[(String, T)]): M[Unit] =
      objectFromIter(iter)

    /** Writes object from the given iterator. */
    private inline def objectFromIter(iter: Iterator[(String, T)]): M[Unit] =
      Objects.writeAll(objectWhitespaces, writeElt, iter, stream)


    /** Wraps to the new line and puts the `indentLevel` indents into the stream. */
    private def wrapAndIndent(): M[Unit] =
      stream.write("\n").flatMap { _ => writeIdents(indentLevel) }


    /** Writes `n` indents. */
    private def writeIdents(n: Int): M[Unit] = {
      if n == 0 then
        Monad.pure(())
      else if n == 1 then
        stream.write(format.indent)
      else
        stream.write(format.indent).flatMap { _ =>
          writeIdents(n-1)
        }
    }
  }


  /** Where we are in the process of printing. */
  enum Context {
    /** We are at the root. */
    case TopLevel
    /** We are inside object (field). */
    case InObject
    /** We are inside array. */
    case InArray

    /** Checks if things should be wrapped based on options and context. */
    def shouldWrap(options: PrettyPrintOptions.WrapEmptyOptions): Boolean =
      this match {
        case TopLevel => options.wrapAtTopLevel
        case InObject => options.wrapInObjects
        case InArray => options.wrapInArrays
      }
  }
}
