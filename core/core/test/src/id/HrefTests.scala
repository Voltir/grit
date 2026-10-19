package grit.core.id

import utest.*

object HrefTests extends TestSuite {

  private def found(text: String): Vector[String] = Href.in(text).map(Href.value)

  val tests = Tests {
    test("in finds a link written `label (url)` and trims a trailing full stop") {
      assert(
        found(
          "See the plan (https://docs.google.com/document/d/abc/edit). Then https://x.org/a."
        ) ==
          Vector("https://docs.google.com/document/d/abc/edit", "https://x.org/a")
      )
    }
    test("in finds each link once, in order, normalised, and only http and https") {
      assert(
        found(
          "https://GitHub.com/o/r and ftp://h/x, then https://github.com/o/r again, `https://b.io/c`"
        ) ==
          Vector("https://github.com/o/r", "https://b.io/c")
      )
    }
    test("in leaves out a link over Chars characters") {
      val long = "https://h.io/" + "a" * Href.Chars
      assert(found(s"$long https://h.io/short") == Vector("https://h.io/short"))
    }
  }
}
