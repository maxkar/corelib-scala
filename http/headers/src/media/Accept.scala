package io.github.maxkar
package http.headers.media

import http.headers.Header
import http.headers.ValueParser
import http.headers.ValueWriter

import scala.collection.mutable.ArrayBuffer

/** Accept header. */
object Accept extends Header[Seq[MediaSelector]]:
  override val name: String = "Accept"

  override def encodeToString(value: Seq[MediaSelector]): String =
    val itr = value.iterator
    if !itr.hasNext then return ""

    val sb = new StringBuilder()
    encodeSelector(sb, itr.next())

    while itr.hasNext do
      sb.append(',')
      encodeSelector(sb, itr.next())
    end while
    sb.toString()
  end encodeToString


  override def decodeFromString(values: Seq[String]): Either[String, Seq[MediaSelector]] =
    if values.isEmpty then return Right(Seq.empty)

    val buf = new ArrayBuffer[MediaSelector]()

    val itr = values.iterator
    while itr.hasNext do
      val err = decodeOneHeader(buf, itr.next())
      if err != null then return Left(err)
    end while
    return Right(buf.toSeq)
  end decodeFromString



  /** Encodes one selector. */
  private def encodeSelector(into: StringBuilder, selector: MediaSelector): Unit =
    into.append(selector.category)
    into.append('/')
    into.append(selector.subtype)
    ValueWriter.writeParameters(selector.parameters, into)
  end encodeSelector


  /** Decodes one header. */
  private def decodeOneHeader(into: ArrayBuffer[MediaSelector], header: String): String =
    val parser = new ValueParser(header)
    parser.skipWhitespaces()

    parser.hasFirstListElement() match
      case Left(err) => return err
      case Right(false) => return null
      case Right(true) => ()
    end match

    var hasMore = true
    while hasMore do
      val proc =
        for
          category <- parser.readToken()
          _ <- parser.expectAndRead('/')
          subtype <- parser.readToken()
          _ <-
            if category != "*" && subtype == "*" then
              Left("Wildcard type must have wildcard category")
            else
              Right(())
          params <- parser.readParameters()
          (notQ, q) = params.partition { p => p._1.equalsIgnoreCase("q") }
          baseWeight <- parseQ(q)
        yield
          if category == "*" then
            MediaSelector.wildcard(baseWeight * 10 + 1)
          else if subtype == "*" then
            MediaSelector.category(category, baseWeight * 10 + 2)
          else
            MediaSelector.full(category, subtype, notQ, baseWeight * 10 + 3)
        end for
      end proc

      proc match
        case Left(err) => return err
        case Right(v) => into.append(v)
      end match
    end while

    return null
  end decodeOneHeader


  /** Parses the "quality" value. */
  private def parseQ(candidates: Seq[(String, String)]): Either[String, Int] =
    if candidates.isEmpty then return Right(1000)
    if candidates.length > 1 then return Left("Too many 'q' parameters in the header")

    val v = candidates.head._2

    if v.length() <= 0 then return Left("Empty 'q' parameter")
    if v.length() > 5 then return Left("Invalid 'q' parameter - too long")

    v.charAt(0) match
      case '0' =>
        var cur = 0
        if v.length() > 1 && v.charAt(1) != '.' then
          return Left("Invalid 'q' parameter - leading 0")
        var ptr = 2
        while ptr < v.length() do
          val c = v.charAt(ptr)
          if c < '0' || '9' < c then
            return Left("Invalid 'q' parameter - not a number")
          cur = cur * 10 + c - '0'
          ptr += 1
        end while

        /* "0.1" is "100" so we have to add a few digits if needed. */
        while ptr < 5 do
          cur = cur * 10
          ptr += 1
        end while

        Right(cur)

      case '1' =>
        if v.length() > 1 && v.charAt(1) != '.' then
          return Left("Invalid 'q' parameter - exceedes 1")
        if v.length() > 2 && v.charAt(2) != '0' then
          return Left("Invalid 'q' parameter - exceedes 1")
        if v.length() > 3 && v.charAt(3) != '0' then
          return Left("Invalid 'q' parameter - exceedes 1")
        if v.length() > 4 && v.charAt(4) != '0' then
          return Left("Invalid 'q' parameter - exceedes 1")
        Right(1000)
      case other => Left("Invalid 'q' parameter - invalid first digit")
    end match
  end parseQ
end Accept
