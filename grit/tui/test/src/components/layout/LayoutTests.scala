package grit.tui.components.layout

import grit.tui.components.View
import grit.tui.model.input.Input
import grit.tui.model.surface.{PaneId, Rect, Size, Surface}
import utest.*

object LayoutTests extends TestSuite {

  import Region.*

  // grit's screen, stated exactly as tui-spike-fullscreen's Layout.of states it: four
  // chrome bands around a flex body, and the body split into transcript | scrollbar.
  private val screen = Stack.of(
    PaneId.of("header") -> Fixed(1),
    PaneId.of("body") -> Flex(1),
    PaneId.of("input") -> Fixed(3),
    PaneId.of("status") -> Fixed(1)
  )

  private val body = Split.of(
    PaneId.of("transcript") -> Flex(1),
    PaneId.of("scrollbar") -> Fixed(1)
  )

  private val Prompt = PaneId.of("prompt")
  private val Body = PaneId.of("body")

  /** A child that wants `want` rows and paints exactly what it is given -- the two
    * halves of the seam, kept apart on purpose.
    */
  private final case class Wants(want: Int) extends View {
    def measure(avail: Size): Size = Size(math.min(avail.rows, want), avail.cols)
    def render(size: Size): Surface = Surface.blank(size)
    type Route = Input
    def route(input: Input, at: Rect): Input = input
  }

  /** grit's screen with the prompt grown by its content instead of pinned at three. */
  private def growing(want: Int, min: Int = 3, upTo: Double = 0.5, floor: Int = 0) =
    Stack
      .of(
        PaneId.of("header") -> Fixed(1),
        Body -> Flex(floor),
        Prompt -> Fit(min, upTo),
        PaneId.of("status") -> Fixed(1)
      )
      .views(Wants(1), Wants(1), Wants(want), Wants(1))

  /** The tiling oracle: every region inside `area`, no two overlapping, and together
    * they cover it exactly -- clamped starvation included, since a starved region is
    * cut short in place rather than painting outside.
    */
  private def assertTiles(area: Rect)(rects: Map[PaneId, Rect]): Unit = {
    assert(rects.nonEmpty)
    val rs = rects.values.toVector
    rs.foreach { r =>
      assert(r.top >= area.top)
      assert(r.left >= area.left)
      assert(r.bottom <= area.bottom)
      assert(r.right <= area.right)
    }
    var i = 0
    while (i < rs.length) {
      var j = i + 1
      while (j < rs.length) {
        val a = rs(i)
        val b = rs(j)
        val overlaps = a.top < b.bottom && b.top < a.bottom &&
          a.left < b.right && b.left < a.right
        assert(!overlaps)
        j += 1
      }
      i += 1
    }
    assert(rs.map(r => r.rows.toLong * r.cols).sum == area.rows.toLong * area.cols)
  }

  val tests = Tests {

    test("grit's screen: the chrome bands tile the terminal, the body takes the rest") {
      val placed = screen.resolve(Size(24, 80))
      assert(placed.starved.isEmpty)
      assertTiles(Rect(0, 0, 24, 80))(placed.rects)
      assert(placed(PaneId.of("header")) == Rect(0, 0, 1, 80))
      assert(placed(PaneId.of("body")) == Rect(1, 0, 19, 80))
      assert(placed(PaneId.of("input")) == Rect(20, 0, 3, 80))
      assert(placed(PaneId.of("status")) == Rect(23, 0, 1, 80))
    }

    test("the body split gives the scrollbar its column, in absolute coordinates") {
      val placed = screen.resolve(Size(24, 80))
      val parts = body.resolveIn(placed(PaneId.of("body")))
      assert(parts.starved.isEmpty)
      assertTiles(placed(PaneId.of("body")))(parts.rects)
      assert(parts(PaneId.of("transcript")) == Rect(1, 0, 19, 79))
      assert(parts(PaneId.of("scrollbar")) == Rect(1, 79, 19, 1))
    }

    test("flex splits the remainder and the first flex absorbs the rounding") {
      val even = Split.of(PaneId.of("a") -> Flex(0), PaneId.of("b") -> Flex(0))
      val five = even.resolve(Size(10, 5))
      assert(five(PaneId.of("a")) == Rect(0, 0, 10, 3))
      assert(five(PaneId.of("b")) == Rect(0, 3, 10, 2))
      val four = even.resolve(Size(10, 4))
      assert(four(PaneId.of("a")) == Rect(0, 0, 10, 2))
      assert(four(PaneId.of("b")) == Rect(0, 2, 10, 2))
    }

    test("a starved region is cut short in place, reported, and never paints outside") {
      val cramped = Stack.of(PaneId.of("a") -> Fixed(3), PaneId.of("b") -> Fixed(2))
      val placed = cramped.resolve(Size(4, 10))
      assert(placed(PaneId.of("a")) == Rect(0, 0, 3, 10))
      assert(placed(PaneId.of("b")) == Rect(3, 0, 1, 10))
      assert(placed.starved == Set(PaneId.of("b")))
      assertTiles(Rect(0, 0, 4, 10))(placed.rects)
      // A demand larger than the whole parent leaves the later regions empty, not
      // displaced: the status bar is gone, not painted one row below the screen.
      val greed = Stack.of(PaneId.of("a") -> Fixed(5), PaneId.of("b") -> Fixed(1))
      val gone = greed.resolve(Size(4, 10))
      assert(gone(PaneId.of("a")) == Rect(0, 0, 4, 10))
      assert(gone(PaneId.of("b")).rows == 0)
      assert(gone.starved == Set(PaneId.of("a"), PaneId.of("b")))
      // A flex short of its minimum is starved the same way; the minimum never steals.
      val minned = Stack.of(PaneId.of("a") -> Flex(3), PaneId.of("b") -> Flex(0))
      val short = minned.resolve(Size(2, 10))
      assert(short(PaneId.of("a")).rows == 1)
      assert(short.starved == Set(PaneId.of("a")))
    }

    test("a fit region is exactly as tall as its child asked to be") {
      // The seam ViewTests calls "cheap enough for a parent to afford to ask": this is
      // the first parent that asks, and the answer is the region's size.
      val placed = growing(want = 6).placed(Size(40, 20))
      assert(placed(Prompt).rows == 6)
      assert(placed(Body).rows == 32)
      assert(placed.starved == Set.empty[PaneId])
    }

    test("it shrinks back down, and never below its own minimum") {
      // Growth that could not reverse would be a prompt that only ever got taller.
      assert(growing(want = 1).placed(Size(40, 20))(Prompt).rows == 3)
      assert(growing(want = 0).placed(Size(40, 20))(Prompt).rows == 3)
    }

    test("`upTo` caps it at a share of the axis, however much the child wants") {
      assert(growing(want = 30).placed(Size(40, 20))(Prompt).rows == 20)
      assert(growing(want = 999).placed(Size(40, 20))(Prompt).rows == 20)
      // A quarter of the same screen, to show the cap is read and not assumed.
      assert(growing(want = 30, upTo = 0.25).placed(Size(40, 20))(Prompt).rows == 10)
    }

    test("a flex sibling's minimum caps it before the share does") {
      // The reason the share is not enough on its own: half of a 12-row screen is 6,
      // and a transcript squeezed to four rows is not a transcript. `Flex(min)` was
      // read only to report starvation until now; here it reserves.
      val roomy = growing(want = 30, floor = 5).placed(Size(40, 20))
      assert(roomy(Prompt).rows == 20) // the share still bites first when it is tighter
      val tight = growing(want = 30, floor = 5).placed(Size(12, 20))
      assert(tight(Prompt).rows == 5)
      assert(tight(Body).rows == 5)
      // and with nothing reserved, the share is all that holds it back.
      assert(growing(want = 30, floor = 0).placed(Size(12, 20))(Prompt).rows == 6)
    }

    test("`min` wins over both caps, and is reported starved when even it cannot fit") {
      // A fit is a demand like any other: cut short in place, never stolen back from a
      // sibling, and the shortfall reported rather than guessed at.
      val squeezed = growing(want = 1, min = 8, floor = 5).placed(Size(12, 20))
      assert(squeezed(Prompt).rows == 8) // past the share (6) and into the body's floor
      assert(squeezed(Body).rows == 2)
      assert(squeezed.starved == Set(Body))
      // On a screen that cannot hold the minimum, a fit degrades exactly as an
      // oversized `Fixed` does: it takes what is left where it stands, and the regions
      // declared after it are cut short and reported rather than compensated.
      val gone = growing(want = 9, min = 9).placed(Size(4, 20))
      assert(gone(Prompt).rows == 3) // all that is left once the header has its row
      assert(gone.starved == Set(Prompt, PaneId.of("status")))
    }

    test("a layout sized by its children refuses to resolve without them") {
      // The alternative is a resolve that guesses and disagrees with what `render`
      // paints -- silently, and only for the app that laid out a fit region.
      val stack = Stack.of(Prompt -> Fit(3, 0.5), Body -> Flex(0))
      val thrown = assertThrows[IllegalArgumentException](stack.resolve(Size(10, 10)))
      assert(thrown.getMessage.contains("Regions.placed"))
      // and the childless path is untouched for every layout that does not hold one.
      assert(screen.resolve(Size(10, 10)).rects.size == 4)
    }

    test("tiling holds at every size for a content-sized layout too") {
      var rows = 0
      while (rows <= 8) {
        var want = 0
        while (want <= 10) {
          val area = Rect(0, 0, rows, 6)
          assertTiles(area)(growing(want, floor = 2).placed(Size(rows, 6)).rects)
          want += 1
        }
        rows += 1
      }
    }

    test("tiling holds at every size, including the starved and the empty") {
      val bodyId = PaneId.of("body")
      var rows = 0
      while (rows <= 8) {
        var cols = 0
        while (cols <= 8) {
          val size = Size(rows, cols)
          val placed = screen.resolve(size)
          assertTiles(Rect(0, 0, rows, cols))(placed.rects)
          val parts = body.resolveIn(placed(bodyId))
          assertTiles(placed(bodyId))(parts.rects)
          cols += 1
        }
        rows += 1
      }
    }
  }
}
