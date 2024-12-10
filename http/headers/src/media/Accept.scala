package io.github.maxkar
package http.headers.media

import http.headers.Header
import http.headers.ValueParser
import http.headers.ValueWriter
import http.headers.HeaderFormatException

import scala.collection.mutable.ArrayBuffer

/** Accept header. */
object Accept extends Header[Seq[MediaSelector]] {
  override val name: String = "Accept"

  override def encodeToString(value: Seq[MediaSelector]): String = {
    val itr = value.iterator
    if !itr.hasNext then return ""

    val sb = new StringBuilder()
    encodeSelector(sb, itr.next())

    while itr.hasNext do {
      sb.append(',')
      encodeSelector(sb, itr.next())
    }
    sb.toString()
  }


  override def decodeFromString(values: Seq[String]): Seq[MediaSelector] = {
    if values.isEmpty then return Seq.empty

    val buf = new ArrayBuffer[MediaSelector]()

    val itr = values.iterator
    while itr.hasNext do
      decodeOneHeader(buf, itr.next())
    buf.toSeq
  }



  /** Encodes one selector. */
  private def encodeSelector(into: StringBuilder, selector: MediaSelector): Unit = {
    into.append(selector.category)
    into.append('/')
    into.append(selector.subtype)
    val allParameters =
      if (selector.weight >= 1000_00)
        selector.parameters
      else
        selector.parameters :+ "q" -> encodeQ(selector.weight / 100)
    ValueWriter.writeParameters(allParameters, into)
  }


  /** Decodes one header. */
  private def decodeOneHeader(into: ArrayBuffer[MediaSelector], header: String): Unit = {
    val parser = new ValueParser(header)
    parser.skipWhitespaces()

    if !parser.hasFirstListElement() then
      return

    var hasMore = true
    while hasMore do {
      val category = parser.readToken()
      parser.expectAndRead('/')
      val subtype = parser.readToken()
      if category == "*" && subtype != "*" then
        throw new HeaderFormatException("Wildcard type must have wildcard category")
      val params = parser.readParameters()
      val (q, notQ) = params.partition { p => p._1.equalsIgnoreCase("q") }
      val baseWeight = parseQ(q)
      val selector =
        if category == "*" then
          MediaSelector.wildcard(baseWeight * 100 + 1)
        else if subtype == "*" then
          MediaSelector.category(category, baseWeight * 100 + 2)
        else {
          val extraWeight = Math.min(notQ.length, 99 - 3)
          MediaSelector.full(category, subtype, notQ, baseWeight * 100 + 3 + extraWeight)
        }
      into.append(selector)
      hasMore = parser.hasNextListElement()
    }
  }


  /** Parses the "quality" value. */
  private def parseQ(candidates: Seq[(String, String)]): Int = {
    if candidates.isEmpty then return 1000
    if candidates.length > 1 then
      throw new HeaderFormatException("Too many 'q' parameters in the header")

    val v = candidates.head._2

    if v.length() <= 0 then
      throw new HeaderFormatException("Empty 'q' parameter")
    if v.length() > 5 then
      throw new HeaderFormatException("Invalid 'q' parameter - too long")

    if v.length() > 1 && v.charAt(1) != '.' then
      throw new HeaderFormatException(s"Invalid q parameter, second char should be '.' but is '${v.charAt(1)}'")

    if v.charAt(0) == '0' then {
      readQDigit(v, 2) * 100 +
      readQDigit(v, 3) * 10 +
      readQDigit(v, 4)
    } else if v.charAt(0) == '1' then {
      if v.length() > 2 then ensureQZero(v, 2)
      if v.length() > 3 then ensureQZero(v, 3)
      if v.length() > 4 then ensureQZero(v, 4)
      1000
    } else
      throw new HeaderFormatException(s"Character '${v.charAt(0)} is not valid start of the Q parameter")
  }


  /** Reads a digit of the Q parameter. */
  private def readQDigit(str: String, offset: Int): Int = {
    if offset >= str.length() then return 0
    val c = str.charAt(offset)
    if c < '0' || '9' < c then
      throw new HeaderFormatException(s"Non-digit '${c}' in the format specifier")
    c - '0'
  }


  /** Ensures that the character is 0. */
  private def ensureQZero(str: String, offset: Int): Unit = {
    val c = str.charAt(offset)
    if c != '0' then
      throw new HeaderFormatException(s"Invalid character '${c} in the q parameter starting with '1.'")
  }


  /** Encodes the quality value that was decoded using this header. */
  private def encodeQ(q: Int): String = {
    if q < 0 then "0"
    else if q < 10 then "0.00" + q.toString()
    else if q < 100 then {
      val tl = if q % 10 == 0 then q / 10 else q
      "0.0" + tl.toString()
    } else if q < 1000 then {
      val tl =
        if q % 100 == 0 then q / 100
        else if q % 10 == 0 then q / 10
        else q
      "0." + tl.toString()
    } else "1"
  }
}
