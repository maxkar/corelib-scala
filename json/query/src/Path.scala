package io.github.maxkar
package json.query

/**
 * Path in the JSON value (i.e. how to navigate from some unspecificed root
 * to a specific element).
 */
abstract sealed class Path:
  import Path._

  /** Append step at the end of the path. */
  def /(step: Step): Path =
    this match
      case Empty => NonEmpty(Vector(step))
      case NonEmpty(prefix) => NonEmpty(prefix :+ step)
    end match
  end /


  /** Concatenates paths. */
  def +(other: Path): Path =
    (this, other) match
      case (Empty, _) => other
      case (_, Empty) => this
      case (NonEmpty(thisSteps), NonEmpty(otherSteps)) => NonEmpty(thisSteps ++ otherSteps)
    end match
  end +


  /** Steps comprising this path (from the "outermost" to innermost). */
  def steps: Seq[Step] =
    this match
      case Empty => Seq.empty
      case NonEmpty(path) => path
    end match
  end steps


  /** Checks if this path is empty. */
  def isEmpty: Boolean = this == Empty


  /** Checks if this path is non-empty. */
  def nonEmpty: Boolean = this != Empty

  /**
   * Returns an iterator over steps of this path.
   */
  def stepIterator: Iterator[Step] =
    this match
      case Empty => Iterator.empty
      case NonEmpty(steps) => steps.iterator
    end match
  end stepIterator

end Path


object Path:
  /** There is no steps to take, the path denotes the "root" object of the hierarchy. */
  private case object Empty extends Path:
    override def toString(): String = "<root>"
  end Empty


  /**
   * A non-empty path which consists of at least one step.
   * @param pathElements steps to take (from the first to last). The steps are specifically
   *   vector as these provide better "append" operations and avoid the need to reverse
   *   elements (if reverse lists are used).
   */
  private case class NonEmpty(pathElements: Vector[Step]) extends Path:
    override def toString(): String =
      val res = new StringBuilder()
      val itr = pathElements.iterator

      res.append(itr.next().encodeNoLeadingDot())
      while itr.hasNext do
        res.append(itr.next().encode())

      res.toString()
    end toString
  end NonEmpty


  /** An "empty" path - the path denotes the object being queried. */
  val empty: Path = Empty

  /** Synonym for the "empty" path - the root object itself. */
  val root: Path = empty

  /**
   * Creates path from its logical steps.
   *
   * The apply() invocation is equivalent to the {{{empty}}} (or {{{root}}}) vector.
   * @param steps steps thas has to be taken in order to get to the given element.
   */
  def apply(steps: Step*): Path =
    if steps.isEmpty then empty else NonEmpty(steps.toVector)
end Path
