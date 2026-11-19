package grit.std.either

import utest.*

object ProvenTests extends TestSuite {

  val tests = Tests {
    test("a Right is its value") {
      Proven.right(Right(3)) ==> 3
    }
    test("a Left is thrown as an IllegalStateException carrying 'ruled out, yet: ' and its text") {
      val thrown = assertThrows[IllegalStateException](Proven.right(Left("no")))
      thrown.getMessage ==> "ruled out, yet: no"
    }
  }
}
