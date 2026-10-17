package grit.core.message

import utest.*

object CutTests extends TestSuite {

  val tests = Tests {
    test("a text over its budget ends at a word's end, then …, within the budget") {
      Vector(
        Cut.head("the deploy moved to Thursday", 14),
        Cut.head("the deploy moved to Thursday", 11),
        Cut.head("the deploy  moved", 13),
        Cut.head("the deploy", 10)
      ) ==> Vector("the deploy…", "the deploy…", "the deploy…", "the deploy")
    }

    test("a word alone longer than the budget is cut inside it, never inside a character") {
      Vector(Cut.head("unbelievably long", 6), Cut.head("ok 😀😀", 5), Cut.head("x", 0)) ==>
        Vector("unbel…", "ok…", "")
      (Cut.head("😀😀", 3), Cut.tail("😀😀", 3)) ==> ("😀…", "…😀")
    }

    test("a text cut from its end starts at a word's start, after …") {
      Vector(
        Cut.tail("the deploy moved to Thursday", 13),
        Cut.tail("the deploy moved to Thursday", 9),
        Cut.tail("unbelievably", 4)
      ) ==> Vector("…to Thursday", "…Thursday", "…bly")
    }
  }
}
