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
 */
final case class JsonFactory[M[_], J](
  trueFactory: TermFactory[M, J],
  falseFactory: TermFactory[M, J],
  nullFactory: TermFactory[M, J],
)
