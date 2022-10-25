package io.github.maxkar
package json.writer

import text.output.Stream

import fun.typeclass.Monad


/** General value output utilities. */
object Values:
  /**
   * Visitor (callback) that knows how to handle a value of the json type.
   * @tparam T type of the JSON DOM value (i.e. element of an array or object).
   * @tparam R visitor's return value.
   */
  trait ValueCallback[-T, +R]:
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

    /** The value is null literal. */
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

  end ValueCallback


  /**
   * "Classifier" (or extractor) from a generic JSON model into the specific
   * types supported by the standard JSON and default printers.
   *
   * @tparam T specific type of JSON model.
   */
  trait ValueClassifier[T]:
    /**
     * Classifies the model value to a specific JSON type and extracts underlying data.
     *
     * @tparam R value returned by the visitor.
     * @param jsonValue value that has to be decoded.
     * @param callback visitor that will (internally) encode the value of the
     *   JSON and return it.
     */
    def classifyValue[R](jsonValue: T, callback: ValueCallback[T, R]): R
  end ValueClassifier


  /**
   * Creates a new compact writer for the given IO monad and model type.
   * The writer would not emit any unnecessary whitespaces.
   *
   * Writer instances are stateless and should be re-used where possible.
   *
   * @tparam M supported IO monad.
   * @tparam T type of the supported JSON model.
   * @param classifier model integration classifier - describes how to map model
   *   types to JSON types and how to extract values/elements from the JSON model.
   * @return reusable writer that is able to produce compact JSON representations.
   */
  def createCompactWriter[M[_]: Monad, T](classifier: ValueClassifier[T]): Writer[M, T] =
    new CompactWriter(classifier)


  /** Writes the value in the most compact form possible. */
  def writeCompact[M[_]: Monad, T, B >: T](
        value: T,
        stream: Stream[M],
      )(implicit
        classifier: ValueClassifier[B],
      ): M[Unit] =
    new CompactWriter.InstanceWriter(classifier, stream).write(value)


  /**
   * Creates a new pretty writer for the given IO monad and model type.
   *
   * Writer instances are stateless and should be re-used where possible.
   *
   * @tparam M supported IO monad.
   * @tparam T type of the supported JSON model.
   * @param format how to format output.
   * @param classifier model integration classifier - describes how to map model
   *   types to JSON types and how to extract values/elements from the JSON model.
   * @return reusable writer that is able to produce compact JSON representations.
   */
  def createPrettyWriter[M[_]: Monad, T](
        format: PrettyPrintOptions,
        classifier: ValueClassifier[T],
      ): Writer[M, T] =
    new PrettyWriter(format, classifier)


  /** Writes the value in the pretty form. */
  def writePretty[M[_]: Monad, T, B >: T](
        format: PrettyPrintOptions,
        value: T,
        stream: Stream[M],
      )(implicit
        classifier: ValueClassifier[B],
      ): M[Unit] =
    new PrettyWriter.InstanceWriter(format, classifier, stream).write(value)
end Values
