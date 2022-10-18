package io.github.maxkar
package json.parser

import fun.typeclass.Monad

type Identity[T] = T

given identityMonad: Monad[Identity] with
  override def pure[T](x: T): T = x

  override def bind[S, R](v: Identity[S], fn: S => Identity[R]): Identity[R] = fn(v)
end identityMonad
