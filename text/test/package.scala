package io.github.maxkar
package text.input

import fun.Monad

/** Identity type for simplifying operations. */
type Identity[T] = T

/** Implementation of the monad for the identity type. */
given identityMonad: Monad[Identity] with
  override def pure[T](x: T): T = x

  override def bind[S, R](v: Identity[S], fn: S => Identity[R]): Identity[R] = fn(v)
end identityMonad
