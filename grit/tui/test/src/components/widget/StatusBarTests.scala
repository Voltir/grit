package grit.tui.components.widget

import grit.tui.model.surface.Size
import utest.*

object StatusBarTests extends TestSuite {

  private def bar(left: Vector[String], right: Vector[String], cols: Int) =
    StatusBar(left, right).render(Size(1, cols))

  val tests = Tests {

    test("left hugs the left edge, right hugs the right edge, one row, whole bar styled") {
      val s = bar(Vector("hello"), Vector("world"), 20)
      assert(s.size == Size(1, 20))
      assert(s.lines(0) == "hello" + " " * 10 + "world")
      assert(s.at(0, 0).style.reverse)
      assert(s.at(0, 10).style.reverse) // the gap is bar too
      assert(s.at(0, 19).style.reverse)
    }

    test("segments join with two spaces") {
      val s = bar(Vector("a", "b"), Vector.empty, 20)
      assert(s.lines(0) == "a  b" + " " * 16)
    }

    test("truncation never wraps: the left segment is cut to the row, the right to what remains") {
      assert(bar(Vector("0123456789abcd"), Vector.empty, 10).lines(0) == "0123456789")
      // The right side yields first: eight columns of left leave two for it.
      assert(bar(Vector("01234567"), Vector("vwxyz"), 10).lines(0) == "01234567vw")
      // No room at all: the right segment is gone, not wrapped onto a second row.
      assert(bar(Vector("0123456789"), Vector("xyz"), 10).lines(0) == "0123456789")
    }

    test("a wide glyph is never cut in half") {
      // Five 世 are ten cells: at eight columns the fourth survives whole.
      assert(bar(Vector("世世世世世"), Vector.empty, 8).lines(0) == "世世世世    ")
      // And the right side's budget is counted in cells, not chars.
      assert(bar(Vector("世世世世"), Vector("xyz"), 10).lines(0) == "世世世世    xy")
    }
  }
}
