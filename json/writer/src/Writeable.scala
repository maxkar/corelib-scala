package io.github.maxkar
package json.writer

/**
 * A trait (typeclass) for JSON DOM models that could be written.
 *
 * The trait provides integration between in-memory model representation (being DOM-like object) and
 * specific json-like output syntax.
 */
trait Writeable[T]:
  /**
   * Decodes one model value as a value of one of the JSON types.
   *
   * The implementation should call one of the visitor's method and return value returned
   * by that method.
   *
   * @tparam R value returned by the visitor.
   * @param jsonValue value that has to be decoded.
   * @param visitor visitor that will (internally) encode the value of the
   *   JSON and return it.
   */
  def decodeElement[R](jsonValue: T, visitor: ValueVisitor[T, R]): R
end Writeable
