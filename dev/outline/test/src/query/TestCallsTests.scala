package grit.outline.query

import grit.outline.model.{Lines, TestCase}

import utest.*

object TestCallsTests extends TestSuite {

  private val src: String = """object S {
  test("first") {
    assert(f("}") == "}") // }
  }
  test("second") { assert(true) }
}
"""

  def tests = Tests {
    test(
      "a test's range ends at the brace closing its body, not at a brace in a string or a comment"
    ) {
      val lines = src.split("\n").toVector
      assert(
        TestCalls.find(src, 0, src.length, "test") == Vector(
          TestCase("first", Lines(2, 4), lines.slice(1, 4).mkString("\n")),
          TestCase("second", Lines(5, 5), lines(4))
        )
      )
    }
  }
}
