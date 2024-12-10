package io.github.maxkar
package http.headers.media

import http.headers.HeaderFormatException

/**
 * Tests for accept header and related functionality.
 * Most tests for headers are "property-based" - they check that the given
 * media types are accepted and not the specific structures are built.
 */
final class AcceptTest extends org.scalatest.funsuite.AnyFunSuite {
  /** Expectation - what to check. */
  private final class Expectation(
        val weight: Int,
        val category: String,
        val subtype: String,
        val params: Seq[(String, String)],
        val accepts: Seq[MediaType],
        val rejects: Seq[MediaType]
      ):
  end Expectation

  /**
   * Just a cookie media type. No RFC examples uses it so most of specific
   * selectors should outright reject it.
   */
  private val cookie = MediaType("food", "sweets", "fill" -> "chocolate", "shape" -> "round")


  test("RFC 7231 - Sample 1") {
    check(
      "audio/*; q=0.2, audio/basic",
      expect(200_02, "audio", "*")(
        MediaType("audio", "fortran"),
        MediaType("audio", "cobol")
      )(
        cookie
      ),
      expect(1000_03, "audio", "basic")(
        MediaType("audio", "basic")
      )(
        MediaType("audio", "fortran"),
        MediaType("audio", "cobol"),
        cookie
      )
    )
  }


  test("RFC 7231 - Sample 2") {
    check(
      "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c",
      expect(500_03, "text", "plain")(
        MediaType("text", "plain"),
      )(
        cookie,
        MediaType("text", "html"),
        MediaType("text", "x-dvi"),
        MediaType("text", "x-c")
      ),
      expect(1000_03, "text", "html")(
        MediaType("text", "html"),
      )(
        cookie,
        MediaType("text", "plain"),
        MediaType("text", "x-dvi"),
        MediaType("text", "x-c")
      ),
      expect(800_03, "text", "x-dvi")(
        MediaType("text", "x-dvi"),
      )(
        cookie,
        MediaType("text", "plain"),
        MediaType("text", "html"),
        MediaType("text", "x-c")
      ),
      expect(1000_03, "text", "x-c")(
        MediaType("text", "x-c"),
      )(
        cookie,
        MediaType("text", "plain"),
        MediaType("text", "html"),
        MediaType("text", "x-dvi"),
      ),
    )
  }


  test("RFC 7231 - Sample 3") {
    check(
      "text/*, text/plain, text/plain;format=flowed, */*",
      expect(1000_02, "text", "*")(
        MediaType("text", "html"),
        MediaType("text", "plain"),
        MediaType("text", "plain", "format" -> "flowed"),
        MediaType("text", "plain", "format" -> "flawed"),
      )(
        cookie,
        MediaType("video", "audio"),
      ),
      expect(1000_03, "text", "plain")(
        MediaType("text", "plain"),
        MediaType("text", "plain", "format" -> "flowed"),
        MediaType("text", "plain", "format" -> "flawed"),
      )(
        cookie,
        MediaType("video", "audio"),
        MediaType("text", "html"),
      ),
      expect(1000_04, "text", "plain", "format" -> "flowed")(
        MediaType("text", "plain", "format" -> "flowed"),
      )(
        cookie,
        MediaType("text", "plain"),
        MediaType("text", "plain", "format" -> "flawed"),
        MediaType("video", "audio"),
        MediaType("text", "html"),
      ),
      expect(1000_01, "*", "*")(
        cookie,
        MediaType("text", "plain"),
        MediaType("text", "plain", "format" -> "flowed"),
        MediaType("text", "plain", "format" -> "flawed"),
        MediaType("video", "audio"),
        MediaType("text", "html"),
      )()
    )
  }


  test("RFC 7231 - Sample 4") {
    check(
      "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5",
      expect(300_02, "text", "*")(
        MediaType("text", "bumpy"),
        MediaType("text", "html"),
        MediaType("text", "html", "level" -> "1"),
        MediaType("text", "html", "level" -> "2"),
        MediaType("text", "html", "level" -> "3"),
      )(
        cookie
      ),
      expect(700_03, "text", "html")(
        MediaType("text", "html"),
        MediaType("text", "html", "level" -> "1"),
        MediaType("text", "html", "level" -> "2"),
        MediaType("text", "html", "level" -> "3"),
      )(
        cookie,
        MediaType("text", "bumpy"),
      ),
      expect(1000_04, "text", "html", "level" -> "1")(
        MediaType("text", "html", "level" -> "1"),
      )(
        cookie,
        MediaType("text", "html"),
        MediaType("text", "bumpy"),
        MediaType("text", "html", "level" -> "2"),
        MediaType("text", "html", "level" -> "3"),
      ),
      expect(400_04, "text", "html", "level" -> "2")(
        MediaType("text", "html", "level" -> "2"),
      )(
        cookie,
        MediaType("text", "html"),
        MediaType("text", "bumpy"),
        MediaType("text", "html", "level" -> "1"),
        MediaType("text", "html", "level" -> "3"),
      ),
      expect(500_01, "*", "*")(
        cookie,
        MediaType("text", "html"),
        MediaType("text", "html", "level" -> "1"),
        MediaType("text", "html", "level" -> "2"),
        MediaType("text", "html", "level" -> "3"),
        MediaType("text", "bumpy"),
      )(
      ),
    )
  }


  test("Bad Q parsing") {
    badQ("text/html;q=10")
    badQ("text/html;q=1.00000")
    badQ("text/html;q=1.999")
    badQ("text/html;q=0.99999")
    badQ("text/html;q=0.abc")
  }


  test("Serialization and deserialization test") {
    /* Basic samples - both RFC (with WS removed) and others. The serialisation
     * should keep the format (it keeps entry and specifier order) and should
     * properly move q to the end.
     */
    val samples =
      Seq(
        "audio/*;q=0.2,audio/basic",
        "text/plain;q=0.5,text/html,text/x-dvi;q=0.8,text/x-c",
        "text/*,text/plain,text/plain;format=flowed,*/*",
        "text/*;q=0.3,text/html;q=0.7,text/html;level=1,text/html;level=2;q=0.4,*/*;q=0.5",
        "image/plain;q=0.5,video/xml;q=0.85,*/*;q=0.885",
        "application/text;a=5;b=c;d=7",
      )

    for sample <- samples do
      withClue(sample) {
        assert(sample === Accept.encodeToString(Accept.decodeFromString(Seq(sample))))
      }
  }


  /**
   * Creates an expectation. Leverages varargs instead of explicit sequences
   * and thus makes the writer code more "consciese"
   */
  private def expect(
        weight: Int,
        category: String,
        subtype: String,
        params: (String, String)*,
      )(
        accepts: MediaType*
      )(
        rejects: MediaType*
      ): Expectation =
    new Expectation(weight, category, subtype, params, accepts, rejects)



  /** Checks that parsing of the given header returns the expected results. */
  private def check(header: String, expectations: Expectation*): Unit =
    withClue(header) {
      val results = Accept.decodeFromString(Seq(header))

      assert(results.length === expectations.length, "Number of selectors should match")

      for
        (selector, expectation) <- results.zip(expectations)
      do {
        assert(selector.weight === expectation.weight, "Weight is not correct")
        assert(selector.category === expectation.category, "Category is not correct")
        assert(selector.subtype === expectation.subtype, "Subtype is not correct")
        assert(selector.parameters === expectation.params, "Params does not match")
        for acc <- expectation.accepts do
          assert(selector.accepts(acc), "Required media is not accepted")
        for rej <- expectation.rejects do
          assert(!selector.accepts(rej), "Media is not rejected")
      }
    }


  /** Checks that invalid Q value raises an exception. */
  def badQ(str: String): Unit =
    withClue(str) {
      assertThrows[HeaderFormatException] {
        Accept.decodeFromString(Seq(str))
      }
    }
}
