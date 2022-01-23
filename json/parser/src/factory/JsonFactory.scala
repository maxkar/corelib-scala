package io.github.maxkar
package json.parser.factory

/**
 * Factory for the various JSON elements.
 * @tparam M operation type.
 * @tparam J representation of JSON value.
 *
 * @param trueFactory factory of the `true` value representation.
 * @param falseFactory factory of the `false` value representation.
 * @param nullFactory factory of the `null` value representation.
 * @param numberFactory factory for numeric values.
 * @param stringFactory factory for string values.
 * @param stringFactory factory for array values.
 * @param objectFactory factory for object values.
 * @param badValue handler for the "bad/unexpected value" situation.
 */
final case class JsonFactory[M[_], J](
  trueFactory: TermFactory[M, J],
  falseFactory: TermFactory[M, J],
  nullFactory: TermFactory[M, J],
  numberFactory: NumberFactory[M, J],
  stringFactory: StringFactory[M, J],
  arrayFactory: ArrayFactory[M, J],
  objectFactory: ObjectFactory[M, J],
  badValue: M[J]
)
