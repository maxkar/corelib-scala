package io.github.maxkar
package json.query

/**
 * Integration between model and the way it is navigated.
 */
trait ModelNavigation[T]:
  /**
   * Performs index access on the given value.
   */
  def index(base: T, index: Int): ModelStepResult[T]

  /**
   * Performs key-based access (i.e. selecting object's field by its name)
   * on the given {{{base}}} value.
   */
  def key(base: T, key: String): ModelStepResult[T]

  /**
   * Performs one step of navigation using the given step.
   */
  final def step(base: T, step: Step): ModelStepResult[T] =
    step match
      case Step.Index(idx) => index(base, idx)
      case Step.Name(nm) => key(base, nm)
    end match
  end step
end ModelNavigation
