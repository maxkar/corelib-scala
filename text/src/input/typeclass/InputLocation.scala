package io.github.maxkar
package text.input.typeclass

/** Information about the current location. */
trait InputLocation[T] {
  /** Returns location of the type T. */
  def get(): T
}

object InputLocation {
  def get[T](using il: InputLocation[T]): T = il.get()
}

