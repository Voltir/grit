package grit.tui.model.surface

import utest.*

object SurfaceTests extends TestSuite {

  /** The glyph oracle: '.' is a blank cell, any other char is that glyph. Every cell
    * of every row must be accounted for, so an unexpected paint anywhere fails. Styles
    * are pinned separately, by masks like [[assertReverse]]. (A literal '.' glyph
    * cannot be spelled; no test needs one yet.)
    */
  private def assertPainted(s: Surface)(rows: String*): Unit = {
    val expected: Vector[Vector[Char]] = rows.toVector.map { r =>
      r.toVector.map { c => if (c == '.') ' ' else c }
    }
    assert(s.size.rows == rows.size)
    assert(s.cells.map(_.ch) == expected.flatten)
  }

  /** The reverse-video mask, in the shape that caught the FINDINGS selection bug:
    * 'x' marks a cell that must be reverse, '.' one that must not be -- every cell
    * of every row.
    */
  private def assertReverse(s: Surface)(rows: String*): Unit = {
    val expected: Vector[Vector[Boolean]] = rows.toVector.map { r =>
      r.toVector.map {
        case 'x' => true
        case '.' => false
        case other => sys.error(s"bad mask char '$other'")
      }
    }
    assert(s.cells.map(_.style.reverse) == expected.flatten)
  }

  val tests = Tests {

    test("write clips at the right edge rather than wrapping") {
      val s = Surface.blank(Size(2, 5)).write(0, 3, "abcdef")
      assertPainted(s)(
        "...ab",
        "....."
      )
    }

    test("write clips a negative column instead of shifting the text") {
      val s = Surface.blank(Size(1, 5)).write(0, -2, "abcdef")
      assertPainted(s)("cdef.")
    }

    test("writes off the grid are dropped, not thrown") {
      val s = Surface.blank(Size(1, 5)).write(9, 0, "x").put(-1, 0, Cell('y'))
      assertPainted(s)(".....")
    }

    test("write replaces exactly its cells and leaves the rest of the row alone") {
      val s = Surface.blank(Size(1, 8)).write(0, 0, "seeded").write(0, 2, "XY")
      assertPainted(s)("seXYed..")
    }

    test("write applies its style to exactly the written range") {
      val s = Surface.blank(Size(2, 6)).write(1, 1, "ab", Style(reverse = true))
      assertPainted(s)(
        "......",
        ".ab..."
      )
      assertReverse(s)(
        "......",
        ".xx..."
      )
    }

    test("fill sets every cell of a rect and clips to the grid") {
      val s = Surface.blank(Size(3, 4)).fill(Rect(2, 2, 5, 5), Cell('#'))
      assertPainted(s)(
        "....",
        "....",
        "..##"
      )
    }

    test("blit composites at an origin and clips") {
      val patch = Surface.filled(Size(2, 2), Cell('#'))
      val s = Surface.blank(Size(3, 4)).blit(patch, Pos(2, 3))
      assertPainted(s)(
        "....",
        "....",
        "...#"
      )
    }

    test("reads outside the grid are total: blank, never thrown") {
      val s = Surface.blank(Size(1, 2)).write(0, 0, "a")
      assert(s.at(0, 2) == Cell.blank)
      assert(s.at(-1, 0) == Cell.blank)
      assert(s.at(0, 0) == Cell('a'))
    }
  }
}
