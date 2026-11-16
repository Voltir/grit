package grit.std.collect

import utest.*
import Collect.firstRepeat

object FirstRepeatTests extends TestSuite {
  val tests = Tests {
    test("the first repeat is the element whose second occurrence comes first") {
      firstRepeat(Vector("a", "b", "b", "a")) ==> Some("b")
    }
    test("a sequence with no element twice, the empty one included, has no repeat") {
      (firstRepeat(Vector("a", "b", "c")), firstRepeat(Vector.empty[String])) ==> (None, None)
    }
  }
}
