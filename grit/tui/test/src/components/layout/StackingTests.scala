package grit.tui.components.layout

import grit.tui.model.surface.{Rect, Size}
import utest.*

/** The stack arithmetic every box of the view tree resolves through. */
object StackingTests extends TestSuite {

  import Region.*

  /** `demands` down an area of `size`, each fit child wanting `want` rows. */
  private def down(size: Size, want: Int, demands: Region*): Vector[Rect] =
    Stacking.rects(
      demands.toVector,
      Rect(0, 0, size.rows, size.cols),
      vertical = true,
      (_, avail) => Size(math.min(avail.rows, want), avail.cols)
    )

  /** grit's screen with the prompt grown by its content: header, body, prompt, status. */
  private def growing(size: Size, want: Int, min: Int = 3, upTo: Double = 0.5, floor: Int = 0) =
    down(size, want, Fixed(1), Flex(floor), Fit(min, upTo), Fixed(1))

  /** The tiling oracle: every rect inside `area`, no two overlapping, and together they
    * cover it exactly -- a demand that cannot be met is cut short in place, never
    * painted outside.
    */
  private def assertTiles(area: Rect)(rs: Vector[Rect]): Unit = {
    rs.foreach { r =>
      assert(r.top >= area.top, r.left >= area.left)
      assert(r.bottom <= area.bottom, r.right <= area.right)
    }
    for (i <- rs.indices; j <- rs.indices if i < j) {
      val (a, b) = (rs(i), rs(j))
      assert(!(a.top < b.bottom && b.top < a.bottom && a.left < b.right && b.left < a.right))
    }
    assert(rs.map(r => r.rows.toLong * r.cols).sum == area.rows.toLong * area.cols)
  }

  val tests = Tests {
    test("flex splits the remainder and the first flex absorbs the rounding") {
      def across(cols: Int) =
        Stacking.rects(Vector(Flex(0), Flex(0)), Rect(0, 0, 10, cols), false, (_, a) => a)
      assert(across(5) == Vector(Rect(0, 0, 10, 3), Rect(0, 3, 10, 2)))
      assert(across(4) == Vector(Rect(0, 0, 10, 2), Rect(0, 2, 10, 2)))
    }

    test("a demand that cannot be met is cut short in place and never paints outside") {
      assert(
        down(Size(4, 10), 0, Fixed(3), Fixed(2)) == Vector(Rect(0, 0, 3, 10), Rect(3, 0, 1, 10))
      )
      // A demand larger than the whole parent leaves the later children empty, not
      // displaced: the status bar is gone, not painted one row below the screen.
      val gone = down(Size(4, 10), 0, Fixed(5), Fixed(1))
      assert(gone(0) == Rect(0, 0, 4, 10), gone(1).rows == 0)
    }

    test("a fit child is exactly as tall as it asked to be") {
      val rs = growing(Size(40, 20), want = 6)
      assert(rs(2).rows == 6, rs(1).rows == 32)
    }

    test("it shrinks back down, and never below its own minimum") {
      // Growth that could not reverse would be a prompt that only ever got taller.
      assert(growing(Size(40, 20), want = 1)(2).rows == 3)
      assert(growing(Size(40, 20), want = 0)(2).rows == 3)
    }

    test("`upTo` caps it at a share of the axis, however much the child wants") {
      assert(growing(Size(40, 20), want = 30)(2).rows == 20)
      assert(growing(Size(40, 20), want = 999)(2).rows == 20)
      assert(growing(Size(40, 20), want = 30, upTo = 0.25)(2).rows == 10)
    }

    test("a flex sibling's minimum caps it before the share does") {
      // Half of a 12-row screen is 6, and a transcript squeezed to four rows is not a
      // transcript: `Flex(min)` reserves.
      assert(growing(Size(40, 20), want = 30, floor = 5)(2).rows == 20)
      val tight = growing(Size(12, 20), want = 30, floor = 5)
      assert(tight(2).rows == 5, tight(1).rows == 5)
      assert(growing(Size(12, 20), want = 30, floor = 0)(2).rows == 6)
    }

    test("`min` wins over both caps") {
      val squeezed = growing(Size(12, 20), want = 1, min = 8, floor = 5)
      assert(squeezed(2).rows == 8, squeezed(1).rows == 2)
      // On a screen that cannot hold the minimum, a fit takes what is left where it
      // stands, and the children after it are cut short.
      val gone = growing(Size(4, 20), want = 9, min = 9)
      assert(gone(2).rows == 3, gone(3).rows == 0)
    }

    test("tiling holds at every size, fit children included") {
      for (rows <- 0 to 8; cols <- 0 to 8; want <- 0 to 10) {
        assertTiles(Rect(0, 0, rows, cols))(growing(Size(rows, cols), want, floor = 2))
      }
    }
  }
}
