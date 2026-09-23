package grit.tui.components.layout

import grit.tui.components.Passive
import grit.tui.model.input.{Input, Key}
import grit.tui.model.surface.{Cell, PaneId, Placements, Pos, Rect, Size, Surface}
import utest.*

object CompositionTests extends TestSuite {

  private final case class Fill(ch: Char) extends Passive {
    def measure(avail: Size): Size = avail
    def render(size: Size): Surface = Surface.filled(size, Cell(ch))
  }

  /** A fill that wants `want` rows, and still paints exactly the box it is given. */
  private final case class Tall(ch: Char, want: Int) extends Passive {
    def measure(avail: Size): Size = Size(math.min(avail.rows, want), avail.cols)
    def render(size: Size): Surface = Surface.filled(size, Cell(ch))
  }

  private val Header = PaneId.of("header")
  private val Body = PaneId.of("body")
  private val Prompt = PaneId.of("prompt")
  private val Left = PaneId.of("left")
  private val Right = PaneId.of("right")

  private val screen = Stack.of(
    Header -> Region.Fixed(1),
    Body -> Region.Flex(3),
    Prompt -> Region.Fixed(3)
  )

  val tests = Tests {

    test("a stack paints its children into the regions it resolved") {
      val v = screen.views(Fill('h'), Fill('b'), Fill('p'))
      val s = v.render(Size(10, 6))
      assert(s.size == Size(10, 6))
      assert(s.lines.head == "hhhhhh")
      assert(s.lines(1) == "bbbbbb")
      assert(s.lines(6) == "bbbbbb")
      assert(s.lines(7) == "pppppp")
      assert(s.lines.last == "pppppp")
    }

    test("a nested resolve places what the nested views paint") {
      // The equality the second description of a layout used to be kept in step with by
      // hand: `nest` resolves what `views` composed. Asserted across sizes, because the
      // two would only diverge where starvation clamps.
      val inner = Split.of(Left -> Region.Flex(1), Right -> Region.Fixed(1))
      val v = screen.views(Fill('h'), inner.views(Fill('l'), Fill('r')), Fill('p'))
      var rows = 0
      while (rows <= 9) {
        var cols = 1
        while (cols <= 9) {
          val size = Size(rows, cols)
          val painted = v.render(size).placements
          val placed = screen.resolve(size).nest(Body, inner)
          // Every region the painting found is where resolving says it is. A region
          // starved to nothing painted nothing and is absent from the map -- resolving
          // still holds an empty rect for it, which is why this reads one way.
          painted.all.foreach { pl => assert(placed.rects.get(pl.pane) == Some(pl.rect)) }
          val painting = painted.all.map(_.pane).toSet
          val innerRect = placed(Left)
          assert(painting.contains(Left) == (innerRect.rows > 0 && innerRect.cols > 0))
          cols += 1
        }
        rows += 1
      }
    }

    test("a content-sized resolve places what the content-sized views paint") {
      // The same equality, for the one layout where the two descriptions could not be
      // kept in step by hand at all: how tall a `Fit` region is is a question only its
      // child can answer, so `placed` is the resolve and `render` is the paint, and
      // this is what says they are the same walk.
      val grows = Stack.of(
        Header -> Region.Fixed(1),
        Body -> Region.Flex(2),
        Prompt -> Region.Fit(min = 3, upTo = 0.5)
      )
      val inner = Split.of(Left -> Region.Flex(1), Right -> Region.Fixed(1))
      var want = 0
      while (want <= 8) {
        val v = grows.views(Fill('h'), inner.views(Fill('l'), Fill('r')), Tall('p', want))
        var rows = 0
        while (rows <= 9) {
          val size = Size(rows, 6)
          val painted = v.render(size).placements
          val placed = v.placed(size).nest(Body, inner)
          painted.all.foreach { pl => assert(placed.rects.get(pl.pane) == Some(pl.rect)) }
          rows += 1
        }
        want += 1
      }
    }

    test("nesting under a pane the layout does not hold changes nothing") {
      val placed = screen.resolve(Size(10, 6))
      assert(placed.nest(PaneId.of("nowhere"), Split.of(Left -> Region.Flex(1))) == placed)
    }

    test("a split lays its children side by side") {
      val v = Split.of(Left -> Region.Flex(1), Right -> Region.Fixed(2)).views(Fill('l'), Fill('r'))
      val s = v.render(Size(2, 7))
      assert(s.lines.forall(_ == "lllllrr"))
    }

    test("every child reports where it landed, by the name of its region") {
      // Composition carries identity: the app names a region once, and hit-testing
      // finds it without the app keeping a parallel map of rects in its own state.
      val v = screen.views(Fill('h'), Fill('b'), Fill('p'))
      val at = Placements(v.render(Size(10, 6)).panes)
      assert(at(PaneId.of("header")).contains(Rect(0, 0, 1, 6)))
      assert(at(PaneId.of("body")).contains(Rect(1, 0, 6, 6)))
      assert(at(PaneId.of("prompt")).contains(Rect(7, 0, 3, 6)))
      assert(at(PaneId.of("nothing")).isEmpty)
    }

    test("nesting translates: an inner region is placed in screen coordinates") {
      // The rects a nested layout reports are absolute, because that is the only
      // coordinate system an input event arrives in.
      val body =
        Split.of(Left -> Region.Flex(1), Right -> Region.Fixed(1)).views(Fill('t'), Fill('s'))
      val v = screen.views(Fill('h'), body, Fill('p'))
      val at = Placements(v.render(Size(10, 8)).panes)
      assert(at(PaneId.of("body")).contains(Rect(1, 0, 6, 8)))
      assert(at(PaneId.of("left")).contains(Rect(1, 0, 6, 7)))
      assert(at(PaneId.of("right")).contains(Rect(1, 7, 6, 1)))
      // and the innermost pane is what a position in it resolves to.
      assert(at.at(Pos(3, 7)).map(_._1).contains(PaneId.of("right")))
      assert(at.at(Pos(3, 2)).map(_._1).contains(PaneId.of("left")))
    }

    test("a position resolves to a pane-local offset, not just a name") {
      val v = screen.views(Fill('h'), Fill('b'), Fill('p'))
      val at = Placements(v.render(Size(10, 6)).panes)
      assert(at.at(Pos(8, 2)) == Some((PaneId.of("prompt"), Pos(1, 2))))
      assert(at.at(Pos(20, 20)).isEmpty)
    }

    test("a composed layout binds nothing of its own") {
      // Heterogeneous children speak different routing vocabularies and a container
      // has no business unifying them -- it reports where they landed and the app
      // dispatches. That is why this is Passive rather than a Route of its own.
      val v = screen.views(Fill('h'), Fill('b'), Fill('p'))
      val key = Input.Keyboard(Key.Enter)
      assert(v.route(key, Rect(0, 0, 10, 6)) == key)
    }

    test("regions and children must correspond") {
      val bad =
        try { screen.views(Fill('h')); false }
        catch { case _: IllegalArgumentException => true }
      assert(bad)
    }

    test("a name painted twice resolves to the topmost, as hit-testing does") {
      // blit appends, so the last placement for a name is the one on top. A lookup
      // that found the first would answer with the rect of something buried under an
      // overlay -- and agree with `at` on nothing.
      val id = PaneId.of("twice")
      val base =
        Surface.blank(Size(6, 6)).blit(Surface.filled(Size(4, 4), Cell('a')), Pos(0, 0), id)
      val over = base.blit(Surface.filled(Size(2, 2), Cell('b')), Pos(3, 3), id)
      val at = Placements(over.panes)
      assert(at(id).contains(Rect(3, 3, 2, 2)))
      assert(at.at(Pos(3, 3)).map(_._1).contains(id))
      // and the two agree about which one is on top.
      assert(at.at(Pos(3, 3)) == Some((id, Pos(0, 0))))
    }

    test("a region starved to nothing is not in the placement map") {
      // `Placed` is total on purpose -- a starved region is still somewhere, as an
      // empty rect -- but `Placements` is the record of what was *painted*, and a
      // zero-area region was not. Keeping them different is the honest split: nothing
      // can be clicked in a region with no cells, and a placement `Hit.paneAt` can
      // never match would be junk in the map.
      val v = screen.views(Fill('h'), Fill('b'), Fill('p'))
      val small = Size(2, 4)
      val at = Placements(v.render(small).panes)
      assert(screen.resolve(small).rects.contains(Body)) // total
      assert(screen.resolve(small)(Body).rows == 0)
      assert(at(PaneId.of("body")).isEmpty) // painted nothing, so placed nowhere
      // What did get cells is still reported, and the layout still tiles.
      assert(at(PaneId.of("header")).contains(Rect(0, 0, 1, 4)))
      assert(at(PaneId.of("prompt")).contains(Rect(1, 0, 1, 4)))
    }

  }
}
