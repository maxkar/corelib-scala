package io.github.maxkar
package http.headers

/** Headers - collection of the "header" elements. */
final class Headers(data: Map[String, (String, Seq[String])] = Map.empty):
  /**
   * Creates an iterator over entries in the headers. The iterator returns
   * header names (usually the first one if multiple headers were set) and
   * their values. The iterator guarantees that the headers are unique (
   * the lowercase forms of all keys are unique).
   */
  def entriesIterator: Iterator[(String, Seq[String])] =
    data.values.iterator


  /**
   * Appends new values and returns new headers with those extra values. Old values
   * are preserved.
   */
  def append(values: (String, String)*): Headers =
    var tmp = data
    for
      (key, value) <- values
    do
      val lowerKey = key.toLowerCase()
      tmp.get(lowerKey) match
        case None =>
          tmp = tmp + (lowerKey -> (key, Seq(value)))
        case Some((outKey, data)) =>
          tmp = tmp + (lowerKey -> (outKey, data :+ value))
    new Headers(tmp)
  end append


  /** Sets/replaces the headers with the new values. */
  def set(values: (String, String)*): Headers =
    new Headers(data ++ Headers.prepare(values))


  /** Removes headers with the given name. */
  def remove(names: String*): Headers =
    new Headers(data -- names.map(_.toLowerCase()))


  /** Removes headers encoded by the header objects. */
  @scala.annotation.targetName("removeHeaders")
  def remove(headers: Header[?]*): Headers =
    new Headers(data -- headers.map(_.name.toLowerCase()))


  /** Checks if the header with the given name is defined. */
  def has(name: String): Boolean = data.contains(name.toLowerCase())


  /** Checks if the header with the given name is defined. */
  @scala.annotation.targetName("hasHeader")
  def has(header: Header[?]): Boolean = data.contains(header.name.toLowerCase())


  /**
   * Returns all header values for the header with the given name. May return
   * an empty list.
   */
  def get(name: String): Seq[String] =
    data.get(name.toLowerCase()).map(_._2).getOrElse(Seq.empty)


  /**
   * Returns the data decoded from the stored values or an error description.
   */
  def get[T](header: Header[T]): T =
    header.decodeFromString(get(header.name))


  /**
   * Returns an optional header.  Returns None if the header is not set otherwise
   * attempts to parse the data into the string.
   */
  def getOpt[T](header: Header[T]): Option[T] =
    data.get(header.name.toLowerCase()).map(v => header.decodeFromString(v._2))


  /**
   * Sets the headers unless these are already set. This method is most commonly
   * used by utilities and factories that set "default" headers. For example, JSON
   * response constructor may set the `Content-Typ` header to `application/json` unless
   * something else is set by the client code.
   */
  def addIfNotSet(values: (String, String)*): Headers =
    var tmp = data
    for
      (k, v) <- values
    do
      val lowerKey = k.toLowerCase()
      if !tmp.contains(lowerKey) then
        tmp = tmp + (lowerKey -> (k, Seq(v)))
    new Headers(tmp)
  end addIfNotSet
end Headers


object Headers:
  /** Empty headers - no data. */
  val empty: Headers = new Headers()

  /**
   * Creates a new set of headers with the given key-value pairs.
   */
  def apply(values: (String, String)*): Headers =
    new Headers(prepare(values).toMap)


  /** Prepares headers for manipulation in the internal form. */
  private def prepare(items: Seq[(String, String)]): scala.collection.MapView[String, (String, Seq[String])] =
    items
      .groupBy(_._1.toLowerCase())
      .view
      .mapValues { entries =>
        val key = entries.head._1
        val values = entries.map(_._2)
        (key, values)
      }
  end prepare
end Headers
