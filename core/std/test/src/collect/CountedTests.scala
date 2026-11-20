package grit.std.collect

import utest.*
import Collect.counted

object CountedTests extends TestSuite {
  val tests = Tests {
    test("each element is counted once, in the order it first occurs") {
      counted(Vector("b", "a", "b", "c", "b")) ==> Vector(("b", 3), ("a", 1), ("c", 1))
    }
    test("the empty sequence counts nothing") {
      counted(Vector.empty[String]) ==> Vector.empty
    }
  }
}
