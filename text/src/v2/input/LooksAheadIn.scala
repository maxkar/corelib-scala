package io.github.maxkar
package text.v2.input

/** A typeclass for objects with look-ahead in a given monad. */
type LooksAheadIn[M[_]] = [T] =>> LookAhead[M, T]
