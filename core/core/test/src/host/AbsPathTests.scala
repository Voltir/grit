package grit.core.host

import utest.*

object AbsPathTests extends TestSuite {

  private def abs(s: String): AbsPath =
    AbsPath.of(s).fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("of: normal form, `..` taking the segment before it and stopping at the root") {
      assert(
        Vector("/a//b/./c/", "/a/b/../c", "/../..", "/").map(AbsPath.of(_).map(AbsPath.value)) ==
          Vector(Right("/a/b/c"), Right("/a/c"), Right("/"), Right("/"))
      )
      assert(AbsPath.of("a/b") == Left("`a/b` is not absolute"))
    }

    test("within: a path is within itself and its ancestors, never a sibling sharing a prefix") {
      assert(AbsPath.within(abs("/a/b"), abs("/a/b")))
      assert(AbsPath.within(abs("/a/b/c"), abs("/a")))
      assert(AbsPath.within(abs("/a"), AbsPath.Root))
      assert(!AbsPath.within(abs("/a/bc"), abs("/a/b")))
    }
  }
}
