package io.github.maxkar
package json.parser

import fun.Monad

/**
 * Parser for json value(s).
 * @tparam M operation type.
 * @tparam J JSON value representation type.
 *
 * @param parserInput input stream/input capabilities.
 * @param jsonFactory json element factory.
 */
class JsonParser[M[_]: Monad, J](
      parserInput: input.ParserInput[M],
      jsonFactory: factory.JsonFactory[M, J]):

  /** Parser for json keys. */
  private val objectKeyParser = new StringParser(parserInput, jsonFactory.objectFactory.keyFactory)

  /** Parser for true literals. */
  private val trueParser = new TermParser("true", parserInput, jsonFactory.trueFactory)
  /** Parser for false literals. */
  private val falseParser = new TermParser("false", parserInput, jsonFactory.falseFactory)
  /** Parser for null literals. */
  private val nullParser = new TermParser("null", parserInput, jsonFactory.nullFactory)
  /** Parser for numbers. */
  private val numberParser = new NumberParser(parserInput, jsonFactory.numberFactory)
  /** Parser for strings. */
  private val stringParser = new StringParser(parserInput, jsonFactory.stringFactory)


  /**
   * Parses single value and raises an error if EOF is reached.
   * @param onEof End-of-file handler.
   */
  def parseSingleValue(onEof: M[J]): M[J] =
    for
      value <- parseValue
      _ <- parserInput.skipWhitespaces()
      isEof <- parserInput.statefulScan(true, checkEof)
      res <- if isEof then onEof else Monad.pure(value)
    yield res


  /**
   * Parses single JSON value. Stops after the value is parsed (the input may
   * contain more json values).
   */
  def parseValue: M[J] =
    for
      _ <- parserInput.skipWhitespaces()
      res <-
        parserInput.lookAhead {
          case 't' => trueParser.parse
          case 'f' => falseParser.parse
          case 'n' => nullParser.parse
          case '"' => stringParser.parse
          case '{' => parseObject
          case '[' => parseArray
          case '-' => numberParser.parse
          case x if Character.isDigit(x) => numberParser.parse
          case _ => jsonFactory.badValue
        }
    yield res


  /** Parses json array value.  */
  private def parseArray: M[J] =
    for
      startState <- jsonFactory.arrayFactory.begin
      _ <- parserInput.skipChar()
      _ <- parserInput.skipWhitespaces()
      res <-
        parserInput.lookAhead {
          case ']' => parserInput.skipChar().flatMap(_ => jsonFactory.arrayFactory.end(startState))
          case _ => parseValue.flatMap { continueArray(startState, _) }
        }
    yield res
  end parseArray


  /** Continues array parsing. */
  private def continueArray(state: jsonFactory.arrayFactory.State, nextElement: J): M[J] =
    val (newState, shouldContinue) = jsonFactory.arrayFactory.update(state, nextElement)
    if (!shouldContinue)
      return jsonFactory.arrayFactory.end(newState)

    for
      _ <- parserInput.skipWhitespaces()
      res <-
        parserInput.lookAhead {
          case ']' => parserInput.skipChar().flatMap(_ => jsonFactory.arrayFactory.end(newState))
          case ',' =>
            for
              _ <- parserInput.skipChar()
              nextVal <- parseValue
              res <- continueArray(newState, nextVal)
            yield res
          case other => jsonFactory.arrayFactory.badArrayContinuation(newState)
        }
    yield res
  end continueArray


  /** Parses json object. */
  private def parseObject: M[J] =
    for
      startState <- jsonFactory.objectFactory.begin
      _ <- parserInput.skipChar()
      _ <- parserInput.skipWhitespaces()
      res <-
        parserInput.lookAhead {
          case '}' => parserInput.skipChar().flatMap(_ => jsonFactory.objectFactory.end(startState))
          case other => continueObject(startState)
        }
    yield res


  /** Continues object parsing .*/
  private def continueObject(state: jsonFactory.objectFactory.State): M[J] =
    parserInput.lookAhead {
      case '"' =>
        for
          key <- objectKeyParser.parse
          _ <- parserInput.skipWhitespaces()
          res <-
            parserInput.lookAhead {
              case ':' => parserInput.skipChar().flatMap{ _ => continueObjectValue(state, key) }
              case _ => jsonFactory.objectFactory.badKeyValueSeparator(state, key)
            }
        yield res
      case _ => jsonFactory.objectFactory.badKeyStart(state)
    }


  /** Continues parsing of object's key-value pair. Considers the colon consumed. */
  private def continueObjectValue(state: jsonFactory.objectFactory.State, key: jsonFactory.objectFactory.Key): M[J] =
    for
      _ <- parserInput.skipWhitespaces()
      value <- parseValue
      (newState, shouldContinue) = jsonFactory.objectFactory.update(state, key, value)
      res <-
        if shouldContinue then
          for
            _ <- parserInput.skipWhitespaces()
            res <-
              parserInput.lookAhead {
                case '}' => parserInput.skipChar().flatMap { _ => jsonFactory.objectFactory.end(newState)}
                case ',' =>
                  for
                    _ <- parserInput.skipChar()
                    _ <- parserInput.skipWhitespaces()
                    r <- continueObject(newState)
                  yield r
                case _ => jsonFactory.objectFactory.badObjectContinuation(newState)
              }
          yield res
        else
          jsonFactory.objectFactory.end(newState)
    yield res



  /** Checks if there is some input in the stream or not. */
  private def checkEof(buffer: CharSequence, isEof: Boolean): (input.ConsumerStatus, Boolean) =
    if !buffer.isEmpty then
      (input.ConsumerStatus.NeedMoreInput, isEof)
    else
      (input.ConsumerStatus.Finished(0), false)

end JsonParser
