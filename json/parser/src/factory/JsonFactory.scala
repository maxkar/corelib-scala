package io.github.maxkar
package json.parser.factory

/**
 * Factory for the various JSON elements.
 * @tparam M operation type.
 * @tparam J representation of JSON value.
 */
trait JsonFactory[M[_], J]:
  /** Factory of the `true` value representation. */
  val trueFactory: TermFactory[M, J]
  /** Factory of the `false` value representation. */
  val falseFactory: TermFactory[M, J]
  /** Factory of the `null` value representation. */
  val nullFactory: TermFactory[M, J]
  /** Factory for numeric values. */
  val numberFactory: NumberFactory[M, J]
  /** Factory for string values. */
  val stringFactory: StringFactory[M, J]
  /** Factory for array values. */
  val arrayFactory: ArrayFactory[M, J]
  /** Factory for object values .*/
  val objectFactory: ObjectFactory[M, J]
  /** Handler for the "bad/unexpected value" situation. */
  def badValue: M[J]
end JsonFactory
