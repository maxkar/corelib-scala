package io.github.maxkar
package json.parser.v3

import fun.typeclass.Monad

/** JSON value readers. */
object Values {
  /** Factory for reading json representation `J` from the stream `S`. */
  trait Factory[S, J] {
    /** Reads the `true` literal. */
    def readTrue(stream: S): J
    /** Reads the `false` literal. */
    def readFalse(stream: S): J
    /** Reads the `null` literal. */
    def readNull(stream: S): J
    /** Reads a number. */
    def readNumber(stream: S): J
    /** Reads a string. */
    def readString(stream: S): J
    /** Reads an array. */
    def readArray(stream: S): J
    /** Reads an object. */
    def readObject(stream: S): J
    /** Handles a situation where no valid value is observed. */
    def invalidValue(stream: S): J
  }


  /** Reads JSON value from the stream. */
  def read[M[_]: Monad, S: Peek.In[M], J](stream: S, factory: Factory[S, M[J]]): M[J] =
    stream.peek(0) >=>> {
      case 't' => factory.readTrue(stream)
      case 'f' => factory.readFalse(stream)
      case 'n' => factory.readNull(stream)
      case '"' => factory.readString(stream)
      case '[' => factory.readArray(stream)
      case '{' => factory.readObject(stream)
      case '-' => factory.readNumber(stream)
      case x if Numbers.isDigitInt(x) => factory.readNumber(stream)
      case _ => factory.invalidValue(stream)
    }
}
