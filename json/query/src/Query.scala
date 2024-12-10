package io.github.maxkar
package json.query

import scala.language.dynamics

import scala.language.implicitConversions

/**
 * A query over some object that has json-like navigation (i.e.
 * property and index access).
 *
 * The query provides fluent javascript-like dynamic navigation (i.e.
 * {{{q.some.field(someIndex)}}}) as well "path" navigation (
 * {{{q / "some" / "field" / someIndex}}} and
 * {{{q / Path("some", "field", someIndex)}}}).
 *
 * The query captures the full path along with the "current" value (if path is correct)
 * or invalid value and what caused the path to be invalid. This information is expected
 * to be used by some implicit conversions provided by companions of the model (providing T)
 * that may leverage both path and model information to perform conversion (for example,
 * capture source of value or provide good error message).
 *
 * @tparam T type of the underlying JSON model.
 */
abstract sealed class Query[T] extends Dynamic {
  /** Navigates to a child of this query. */
  def /(step: Step)(using ModelNavigation[T]): Query[T]

  /** Navigates to some (maybe distant) child of this query. */
  def /(path: Path)(using ModelNavigation[T]): Query[T]

  /**
   * Navigates over the simple named element.
   *
   * Usage:
   * {{{
   *   value.field
   * }}}
   */
  def selectDynamic(name: String)(using ModelNavigation[T]): Query[T] = this / name

  /**
   * Navigates over the simple named element. Synonym to dynamic access but
   * could be used with names that are not valid identifiers.
   *
   * Usage:
   * {{{
   *   value("field")
   * }}}
   */
  def apply(name: String)(using ModelNavigation[T]): Query[T] = this / name


  /**
   * Navigates over simple element. Synonym to dynamic access but
   * could be used with names that are not valid identifiers.
   *
   * Usage:
   * {{{
   *   value(index)
   * }}}
   */
  def apply(index: Int)(using ModelNavigation[T]): Query[T] = this / index

  /**
   * Navigates over the sequence of steps.
   */
  def apply(steps: Step*)(using ModelNavigation[T]): Query[T] = this / Path(steps*)


  /**
   * More complex format that makes it possible to combine "field" and index navigation
   * or simple and complex names.
   * Usage:
   * {{{
   *   value.arr(5)
   *   value.someObj("field with space")
   * }}}
   */
  def applyDynamic(name: String, args: Step*)(using ModelNavigation[T]): Query[T] = this / (Path((name +: args)*))


  /**
   * Converts this query into some concrete value.
   * @tparam R resulting type.
   */
  def as[R](implicit conv: Conversion[Query[T], R]): R =
    conv.convert(this)
}


/**
 * Associated query definitions and query utilities. The Query class provides a very
 * minimal set of supported methods to allow those names be used as "json keys" with
 * dot notation (i.e. value.field). The special methods are supposed to be used by
 * the specific json modules (json model integrations) so this approach won't be a huge
 * inconvenience.
 */
object Query {
  /**
   * The query is valid and corresponds to the given value (model element).
   * @tparam T type of the underlying model.
   * @tparam path path captured so far.
   * @tparam value value at the given path.
   */
  case class ValidQuery[T](path: Path, value: T) extends Query[T] {
    override def /(step: Step)(implicit mn: ModelNavigation[T]): Query[T] =
      mn.step(value, step) match {
        case ModelStepResult.Success(s) => ValidQuery(path / step, s)
        case ModelStepResult.IllegalSelector =>
          InvalidSelector(path, value, Path(step))
        case ModelStepResult.MissingValue =>
          MissingElement(path, value, Path(step))
      }

    override def /(path: Path)(implicit mn: ModelNavigation[T]): Query[T] = {
      var curPath = this.path
      var curValue = value
      val itr = path.stepIterator
      while itr.hasNext do {
        val curStep = itr.next
        mn.step(curValue, curStep) match {
          case ModelStepResult.Success(s) =>
            curPath = curPath / curStep
            curValue = s
          case ModelStepResult.IllegalSelector =>
            return InvalidSelector(curPath, curValue, Path(curStep) + Path(itr.toSeq*))
          case ModelStepResult.MissingValue =>
            return MissingElement(curPath, curValue, Path(curStep) + Path(itr.toSeq*))
        }
      }
      ValidQuery(curPath, curValue)
    }
  }


  /**
   * A query that is not valid. It contains the last valid element, path to that
   * element and the invalid path (from the valid element) that was accumulated.
   * @tparam T type of the underlying model.
   */
  abstract sealed class InvalidQuery[T] extends Query[T]

  /**
   * Path where some selector was "out of range" of the given element. The "out of range"
   * denotes index out of bounds (for arrays) or non-existent key (for objects).
   *
   * Query of this type implies that access was valid for the given value (i.e. index
   * for array or string key for json object).
   *
   * @param validPath path to the valid value.
   * @param validValue last value observed on the path.
   * @param invalidPath path since the last valid value.
   */
  case class MissingElement[T](validPath: Path, validValue: T, invalidPath: Path) extends InvalidQuery[T] {
    override def /(step: Step)(implicit mn: ModelNavigation[T]): Query[T] =
      copy(invalidPath = invalidPath / step)

    override def /(path: Path)(implicit mn: ModelNavigation[T]): Query[T] =
      copy(invalidPath = invalidPath + path)
  }


  /**
   * Path where some selector is not applicable to the actual element type
   * (for example, index access for non-array value).
   *
   * @param validPath path prefix to the valid value.
   * @param validValue last valid value observed.
   * @param invalidPath path that was requested to apply at the {{{vaildValue}}} but could
   *   not be used as selector is not applicable to the actual type.
   */
  case class InvalidSelector[T](validPath: Path, validValue: T, invalidPath: Path) extends InvalidQuery[T] {
    override def /(step: Step)(implicit mn: ModelNavigation[T]): Query[T] =
      copy(invalidPath = invalidPath / step)

    override def /(path: Path)(implicit mn: ModelNavigation[T]): Query[T] =
      copy(invalidPath = invalidPath + path)
  }


  /**
   * Returns full path specified by the selector.
   */
  def fullPath(q: Query[?]): Path =
    q match {
      case ValidQuery(path, _) => path
      case MissingElement(validPath, _, invalidPath) => validPath + invalidPath
      case InvalidSelector(validPath, _, invalidPath) => validPath + invalidPath
    }

  /** Creates a new query using the given value as root. */
  def apply[T](v: T): Query[T] =
    ValidQuery(Path.empty, v)
}
