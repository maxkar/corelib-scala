package io.github.maxkar
package json.parser

import fun.instances.Identity
import fun.instances.Identity.given

/** Tests for end-of-file-related. */
final class EndOfFileTest extends org.scalatest.funsuite.AnyFunSuite {
  import EndOfFileTest._
  import EndOfFileTest.given


  test("Immediate end-of-file (success)") {
    val stream = new SimpleStringStream("", 1)
    EndOfFile.expectImmediately(stream)
  }


  test("Immediate end-of-file (failure)") {
    val stream = new SimpleStringStream(" ", 1)
    try {
      EndOfFile.expectImmediately(stream)
      fail("An error has to be raised")
    } catch {
      case e: EofErrors.EofExpected =>
        assert(stream.readOffset === 0)
    }
  }


  test("Non-immediate end of file (success)") {
    val validSequences = Seq(
      "",
      "   ",
      "\r\n",
      " \n  \r  \n\r   \r\n   "
    )
    for
      inputString <- validSequences
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        EndOfFile.expectNoValues(stream)
        assert(stream.readOffset === inputString.length())
      }
  }


  test("Non-immediate end of file (failure)") {
    val validSequences = Seq(
      "",
      "   ",
      "\r\n",
      " \n  \r  \n\r   \r\n   "
    )

    val trailers = Seq(
      ",",
      "true",
      "false",
      "{}",
      "cat"
    )
    for
      prefix <- validSequences
      trailer <- trailers
      inputString = prefix + trailer
      chunkSize <- 1 until inputString.length()
    do
      withClue(s"${inputString} (by ${chunkSize})") {
        val stream = new SimpleStringStream(inputString, chunkSize)
        try {
          EndOfFile.expectNoValues(stream)
          fail("An error has to be raised")
        } catch {
          case e: EofErrors.EofExpected =>
            assert(stream.readOffset === prefix.length())
        }
      }
  }
}


object EndOfFileTest {
  /** Implementation of end-of-file errors. */
  given EofErrors: EndOfFile.Errors[Identity, Any] with {
    /** Encoding for missing end-of-file. */
    case class EofExpected() extends Exception

    override def endOfFileExpected(stream: Any): Identity[Unit] =
      throw new EofExpected()
  }
}
