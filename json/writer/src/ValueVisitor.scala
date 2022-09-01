package io.github.maxkar
package json.writer


/**
 * Visitor (callback) that knows how to handle a value of the json type.
 * @tparam T type of the JSON DOM value (i.e. element of an array or object).
 * @tparam R visitor's return value.
 */
trait ValueVisitor[T, R]:
  /**
   * The value is represented as json boolean.
   * @param v value as a boolean.
   */
  def boolean(v: Boolean): R


  /**
   * The value is JSON number. The method takes value **representation** and
   * not specific type because application may use some custom types internally.
   * For all primitive types and BigInt/BigDecimal varations the `value.toString()`
   * could be safely passed to this method.
   * @param representation JSON representation of the number.
   */
  def number(representation: CharSequence): R


  /**
   * The value is JSON string.
   * @param v value as string.
   */
  def string(v: CharSequence): R


  /**
   * The value is null literal.
   */
  def nullValue(): R


  /**
   * The value is JSON array.
   * @param iter iterator over values in the array.
   */
  def array(iter: Iterator[T]): R


  /**
   * The value is JSON object (key-value sequence). The iteration may be unordered.
   * @param iter iterator over key/value pairs.
   */
  def unorderedObject(iter: Iterator[(String, T)]): R


  /**
   * The value is JSON object and implementation guarentees that the iterator returns
   * keys in the lexicographical order. Implementations using ordeder structures (for example,
   * ordered maps) are encouraged to call this method as it may improve performance of pretty
   * printing.
   * @param iter iterator over key/value pairs in the lexicographical order.
   */
  def orderedObject(iter: Iterator[(String, T)]): R =
    unorderedObject(iter)

end ValueVisitor
