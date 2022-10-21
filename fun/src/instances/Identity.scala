package io.github.maxkar
package fun.instances

import fun.typeclass.Monad
import fun.typeclass.Collect

/** Identity type constructor. */
type Identity[T] = T

object Identity:
  /** Implementation of Monad for Identity class. */
  given identityMonad: Monad[Identity] with
    override def pure[T](v: T): T = v
    override def bind[S, R](v: S, fn: S => R): R = fn(v)
    override def aapply[S, R](v: S, fn: S => R): R = fn(v)
    override def fmap[S, R](v: S, fn: S => R): R = fn(v)
    override def flatten[T](v: T): T = v
  end identityMonad


  /** Implementation of the "Collect" typeclass for the identity type. */
  given identityCollect: Collect[Identity] with
    override def collect[T](items: Seq[T]): Seq[T] = items
  end identityCollect
end Identity
