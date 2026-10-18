package grit.core.document

import utest.*

object DocRefTests extends TestSuite {

  val tests = Tests {
    test("a kind is lower-case words and digits joined by '-'") {
      Vector("strand", "card-2", "a-b-c", "Strand", "card_2", "-a", "a-", "a--b", "")
        .map(DocRef.of(_, "x").isRight) ==>
        Vector(true, true, true, false, false, false, false, false, false)
    }

    test("a ref is 1 to 512 characters, counted as code points, none of them NUL") {
      Vector("", "a" * 512, "a" * 513, "😀" * 512, "a\u0000b")
        .map(DocRef.of("strand", _).isRight) ==> Vector(false, true, false, true, false)
    }
  }
}
