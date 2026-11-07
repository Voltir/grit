package grit.prose.words

import utest.*

/** [[Words]]: the English grit writes alike everywhere. */
object WordsTests extends TestSuite {

  val tests = Tests {
    test("a count names its unit in the singular for one alone") {
      (Words.counted(0, "row"), Words.counted(1, "row"), Words.counted(3, "row")) ==>
        ("0 rows", "1 row", "3 rows")
    }
  }
}
