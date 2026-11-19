package grit.std.collect

import utest.*
import Collect.positions

object PositionsTests extends TestSuite {
  val tests = Tests {
    test("a needle's positions are every index it starts at, overlapping ones included") {
      (positions("aaa", "aa"), positions("abcab", "ab"), positions("abc", "d")) ==>
        (Vector(0, 1), Vector(0, 3), Vector())
    }
    test("an empty needle is at no position") {
      (positions("abc", ""), positions("", "")) ==> (Vector(), Vector())
    }
  }
}
