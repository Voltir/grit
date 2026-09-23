package grit.tui.components.widget

import grit.tui.components.pane.{Anchor, Panes, TextPane}
import grit.tui.model.select.Doc
import grit.tui.model.surface.{PaneId, Placement, Placements, Pos, Rect, Size}
import grit.tui.model.text.WrapCache
import utest.*

/** The composite, asserted against the pane's own state -- never against a remembered
  * rect. Every question here is one an app used to answer for itself.
  */
object ScrollPaneTests extends TestSuite {

  private val Content = PaneId.of("content")
  private val Bar = PaneId.of("bar")

  private val sp = ScrollPane(Content, Bar)

  /** Forty short lines in a six-row window: enough document that the thumb travels. */
  private def panes: Panes =
    Panes
      .of(
        TextPane(
          Content,
          Doc.of((0 until 40).map(i => f"line$i%02d")*),
          cache = WrapCache.empty(20)
        )
      )
      .layout(Content, Size(6, 20))

  /** Where the last frame put the pair: six rows, the track in the last column. */
  private val at = Placements(
    Vector(Placement(Content, Rect(0, 0, 6, 19)), Placement(Bar, Rect(0, 19, 6, 1)))
  )

  private def topRow(p: Panes): Option[Int] =
    p.rendered(Content).top.flatMap(p.rowIndex(Content).indexOf)

  val tests = Tests {

    test("the split gives the document what is left and the track one column") {
      // Asserted through `resolveIn` rather than against `split.regions`: reading the
      // regions back is a restatement of the one line that declares them, and says
      // nothing about the columns they resolve to.
      val placed = sp.split.resolveIn(Rect(0, 0, 6, 20))
      assert(placed(Bar) == Rect(0, 19, 6, 1))
      assert(placed(Content) == Rect(0, 0, 6, 19))
    }

    test("the wheel over the track scrolls what the track tracks, and nothing else does") {
      assert(sp.wheelTarget(Bar) == Some(Content))
      assert(sp.wheelTarget(Content) == None)
      assert(sp.wheelTarget(PaneId.of("elsewhere")) == None)
    }

    test("the bar reads the pane's own geometry, at the height it was last painted at") {
      val p = panes
      val bar = sp.scrollbar(p)
      assert(bar.content == p.rowIndex(Content).length)
      assert(bar.window == p.rendered(Content).size.rows)
      assert(bar.window == 6)
    }

    test("a grab at the head of the track reads from the top, at the foot from the end") {
      // The pane starts anchored at the bottom, so the head of the track is a real move.
      val head = sp.grabbed(panes, Pos(0, 19), at)
      assert(topRow(head) == Some(0))
      val foot = sp.grabbed(panes, Pos(5, 19), at)
      assert(topRow(foot).exists(_ > 0))
      // Monotone down the track: no row reads from further up than the row above it.
      var previous = -1
      var row = 0
      while (row < 6) {
        val here = topRow(sp.grabbed(panes, Pos(row, 19), at)).getOrElse(0)
        assert(here >= previous)
        previous = here
        row += 1
      }
    }

    test("a grab never starts a selection: that is the whole point of the composite") {
      val grabbed = sp.grabbed(panes, Pos(3, 19), at)
      assert(grabbed.drag.isEmpty)
      assert(grabbed.selectionIn(Content).isEmpty)
      // And it moved by re-anchoring, which is the only thing it is allowed to do.
      assert(grabbed.get(Content).exists(_.anchor != Anchor.Bottom))
    }

    test("a track that was never painted leaves the panes exactly as they were") {
      val p = panes
      assert(sp.grabbed(p, Pos(3, 19), Placements(Vector())) == p)
    }

    test("the pair paints as one view: the document, and the track beside it") {
      val s = sp.views(panes).render(Size(6, 20))
      assert(s.size == Size(6, 20))
      // The last column is the track -- rail or thumb, never document text.
      val track = (0 until 6).map(r => s.lines(r).charAt(19)).toSet
      assert(track.subsetOf(Set('│', '█')))
      // And both halves are findable by name in the next onInput.
      val places = s.placements.all.map(_.pane).toSet
      assert(places.contains(Content) && places.contains(Bar))
    }
  }
}
