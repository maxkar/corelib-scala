package io.github.maxkar
package text.v2.input

/** Type that has a read operation in the specific monad. */
type ReadsIn[M[_]] = [T] =>> Reader[M, T]

