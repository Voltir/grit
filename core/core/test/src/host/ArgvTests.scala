package grit.core.host

import utest.*

/** [[Argv.quote]]: which words a shell reads back as they are, and how every other is quoted. */
object ArgvTests extends TestSuite {

  val tests = Tests {
    test("quote leaves a word of letters, digits and _@%+:,./- as it is") {
      Argv.quote("Az09_@%+:,./-") ==> "Az09_@%+:,./-"
    }
    test("quote single-quotes a word holding any other character") {
      Vector("a=b", "café", "a b", "$x", "a\nb", "~", "a\u0000b").map(Argv.quote) ==>
        Vector("'a=b'", "'café'", "'a b'", "'$x'", "'a\nb'", "'~'", "'a\u0000b'")
    }
    test("quote writes a single quote as a closed quote, an escaped one and a reopened one") {
      Argv.quote("it's") ==> "'it'\\''s'"
    }
    test("quote writes an empty word as two single quotes") {
      Argv.quote("") ==> "''"
    }
  }
}
