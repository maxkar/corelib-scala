package io.github.maxkar
package json.parser

import fun.typeclass.Functor
import fun.typeclass.Monad

/** String parsers. */
object Strings {
  /**
   * Factory that creates a JSON representation `J` of a string
   * from the stream `S`.
   */
  trait Factory[M[_], -S, J] {
    /** Context that is used to capture string contents. */
    type Context

    /**
     * Starts parsing the sting and consumer "string start" marker.
     * @param stream stream that is being read.
     * @param count number of characters in the string start marker.
     * @return context for parsing this specific string.
     */
    def start(stream: S, count: Int): M[Context]

    /**
     * Handles an error where a string is expected but no string start
     * marker is present.
     */
    def invalidStringStart(stream: S): M[Context]

    /**
     * Consumes string characters while they match `predicate` and
     * add them to the string.
     */
    def readWhile(stream: S, context: Context, predicate: Char => Boolean): M[Unit]

    /**
     * Consumes one escaped character from the stream and adds it to
     * @param context context of string parsing.
     * @param count number of characters that comprise the escape character.
     * @param char unescaped character.
     */
    def readEscape(stream: S, context: Context, count: Int, char: Char): M[Unit]

    /**
     * Invalid escape character occured while reading the string. The stream position
     * is before the escape character ('\').
     * The implementation should either complete abruptly or consume at least one
     * character from the stream.
     */
    def invalidEscapeCharacter(stream: S, context: Context): M[Unit]

    /**
     * Invalid escape character occured while reading the string. The stream position
     * is before the escape character ('\').
     * The implementation should either complete abruptly or consume at least one
     * character from the stream.
     */
    def invalidUnicodeEscape(stream: S, context: Context): M[Unit]

    /**
     * Invalid character occured while reading the string.
     * The implementation should either complete abruptly or consume at least one
     * character from the stream.
     */
    def invalidCharacter(stream: S, context: Context): M[Unit]

    /** Finishes string consumption and returns the string representation. */
    def finish(stream: S, context: Context, count: Int): M[J]

    /** Indicates that the string was not terminated properly but end of stream was reached. */
    def invalidStringEnd(stream: S, context: Context): M[J]
  }


  object Factory {
    abstract class RaiseParseErrors[M[_], -S: ParseError.In[M], J] extends Factory[M, S, J] {
      override final def invalidStringStart(stream: S): M[Context] =
        stream.parseError("Invalid string start")

      override final def invalidEscapeCharacter(stream: S, context: Context): M[Unit] =
        stream.parseError("Invalid escape character")

      override final def invalidUnicodeEscape(stream: S, context: Context): M[Unit] =
        stream.parseError("Invalid unicode escape")

      override final def invalidCharacter(stream: S, context: Context): M[Unit] =
        stream.parseError("Invalid character")

      override final def invalidStringEnd(stream: S, context: Context): M[J] =
        stream.parseError("Invalid string end")
    }


    abstract class Simple[M[_]: Functor, -S: SkipStream.In[M]: ParseError.In[M], J] extends RaiseParseErrors[M, S, J] {
      /** Creates a context. */
      def createContext(): Context

      /** Converts context to json value. */
      def createValue(context: Context): J

      /** Appends a simple character to the context. */
      def append(context: Context, chr: Char): Unit

      override final def start(stream: S, count: Int): M[Context] =
        stream.skip(count) >-| createContext()

      override final def readEscape(stream: S, context: Context, count: Int, char: Char): M[Unit] =
        stream.skip(count) >-| append(context, char)

      override final def finish(stream: S, context: Context, count: Int): M[J] =
        stream.skip(count) >-| createValue(context)
    }


    /** Reader of the string context as a simple string. */
    final class AsString[M[_]: Functor, -S: DefaultStream.In[M]: ParseError.In[M]] extends Simple[M, S, String] {
      override type Context = StringBuilder

      override def createContext(): Context =
        new Context()
      override def append(context: Context, chr: Char): Unit =
        context += chr
      override def readWhile(stream: S, context: Context, predicate: Char => Boolean): M[Unit] =
        stream.readWhile(context, predicate)
      override def createValue(context: StringBuilder): String =
        context.toString()
    }
  }


  /** Reads string from the stream. */
  def read[M[_]: Monad, S: Peek.In[M], J](factory: Factory[M, S, J])(stream: S): M[J] =
    stream.peek(0) >=>> { c =>
      if c == '"' then factory.start(stream, 1) else factory.invalidStringStart(stream)
    } >=>> readBody(stream, factory)


  /** Reads string body (after the opening marker). */
  private def readBody[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
      )(
        ctx: factory.Context
      ): M[J] =
    def readRec(): M[J] =
      factory.readWhile(stream, ctx, isRegularCharacter) >=||
      stream.peek(0) >=>> {
        case '"' => factory.finish(stream, ctx, 1)
        case '\\' => readEscape(stream, factory, ctx) >=|| readRec()
        case x if x <  0 => factory.invalidStringEnd(stream, ctx)
        case other => factory.invalidCharacter(stream, ctx) >=|| readRec()
      }
    readRec()


  /** Reads one escape character. */
  private def readEscape[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        ctx: factory.Context
      ): M[Unit] =
    stream.peek(1) >=>> {
      case '"' => factory.readEscape(stream, ctx, 2, '"')
      case '\\' => factory.readEscape(stream, ctx, 2, '\\')
      case '/' => factory.readEscape(stream, ctx, 2, '/')
      case 'b' => factory.readEscape(stream, ctx, 2, '\b')
      case 'f' => factory.readEscape(stream, ctx, 2, '\f')
      case 'n' => factory.readEscape(stream, ctx, 2, '\n')
      case 'r' => factory.readEscape(stream, ctx, 2, '\r')
      case 't' => factory.readEscape(stream, ctx, 2, '\t')
      case 'u' => readUnicodeEscape(stream, factory, ctx)
      case other => factory.invalidEscapeCharacter(stream, ctx)
    }


  /** Reads one unicode escape. */
  private def readUnicodeEscape[M[_]: Monad, S: Peek.In[M], J](
        stream: S,
        factory: Factory[M, S, J],
        ctx: factory.Context
      ): M[Unit] =
    for
      c1 <- peekHexDigit(stream, 2)
      c2 <- peekHexDigit(stream, 3)
      c3 <- peekHexDigit(stream, 4)
      c4 <- peekHexDigit(stream, 5)
      res <-
        if (c1 | c2 | c3 | c4) < 0 then
          factory.invalidUnicodeEscape(stream, ctx)
        else
          factory.readEscape(stream, ctx, 6, ((c1 << 12) | (c2 << 8) | (c3 << 4) | c4).toChar)
    yield res


  /**
   * Peeks one hex digit.
   * Returns negative value if character is not a valid digit.
   */
  private def peekHexDigit[M[_]: Monad, S: Peek.In[M], J](stream: S, offset: Int): M[Int] =
    stream.peek(offset) >-> {
        case d if '0' <= d && d <= '9' => d - '0'
        case d if 'a' <= d && d <= 'f' => 10 + d - 'a'
        case d if 'A' <= d && d <= 'F' => 10 + d - 'A'
        case _ => -1
    }


  /** Checks if the given character is regular and valid string character. */
  def isRegularCharacter(char: Char): Boolean =
    char match {
      case '\\' | '"' | '\r' | '\n' => false
      case x if x < 0x20 => false
      case _ => true
    }
}
