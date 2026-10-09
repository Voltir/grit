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

    test(
      "a call wrapped across lines is found from the line of its call, and its body opens on the line of its closing paren"
    ) {
      val wrapped = """object S {
  test(
    "wrapped form"
  ) {
    assert(true)
  }
  test("one line") { assert(true) }
}
"""
      val lines = wrapped.split("\n").toVector
      assert(
        TestCalls.find(wrapped, 0, wrapped.length, "test") == Vector(
          TestCase("wrapped form", Lines(2, 6), lines.slice(1, 6).mkString("\n")),
          TestCase("one line", Lines(7, 7), lines(6))
        )
      )
    }
  }
}
