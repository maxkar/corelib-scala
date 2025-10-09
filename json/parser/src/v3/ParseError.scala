package io.github.maxkar
package json.parser.v3

/** A typeclass for raising parsing errors in a context of a stream `S`. */
trait ParseError[M[_], -S] {
  extension (stream: S) {
    def parseError[T](message: String): M[T]
  }
}


object ParseError {
  type In[M[_]] = [S] =>> ParseError[M, S]
  type Of[S] = [M[_]] =>> ParseError[M, S]
}
