package io.github.maxkar
package http.headers

/** Utilities for writing header values. */
object ValueWriter {
  /** Appends string to the string builder. */
  def writeString(str: String, into: StringBuilder): Unit = {
    var ptr = 0
    into.append('"')
    while ptr < str.length() do {
      val c = str.charAt(ptr)
      if !ValueParser.isValidStringCharacter(c) then
        into.append('\\')
      into.append(c)

      ptr += 1
    }

    into.append('"')
  }


  /** Appends the text as either string or token. */
  def writeStringOrToken(str: String, into: StringBuilder): Unit =
    if str.forall(ValueParser.isTokenChar) && str.nonEmpty then
      into.append(str)
    else
      writeString(str, into)


  /** Appends parameters - key-value pairs. */
  def writeParameters(params: Iterable[(String, String)], into: StringBuilder): Unit = {
    val itr = params.iterator
    while itr.hasNext do {
      into.append(';')
      val (k, v) = itr.next
      into.append(k)
      into.append('=')
      writeStringOrToken(v, into)
    }
  }
}
