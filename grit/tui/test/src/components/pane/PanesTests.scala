package grit.tui.components.pane

import grit.tui.model.block.Block
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.model.surface.{PaneId, Placements, Pos, Rect, Size, Surface}
import grit.tui.model.text.WrapCache
import utest.*

/** The registry, and the rule it exists to make structural: a drag belongs to the pane it
  * began in.
  *
  * The layout under test is a transcript with a modal composited over its left half --
  * the arrangement in which a selection escaping into what is behind it is both easy to
  * write and invisible in a screenshot.
  */
object PanesTests extends TestSuite {

  private val back = PaneId.of("transcript")
  private val front = PaneId.of("modal")

  private val size = Size(10, 20)

  private def transcript: TextPane =
    TextPane(back, Doc.of("alpha", "bravo", "charlie", "delta"), cache = WrapCache.empty(20))

  private def modal: TextPane =
    TextPane(front, Doc.of("yes", "no"), cache = WrapCache.empty(8))

  /** The transcript across the whole screen with the modal blitted over rows 2-5,
    * columns 2-9 -- and the panes laid out at the sizes they were painted at.
    */
  private def scene: (Panes, Surface) = {
    val panes = Panes
      .of(transcript, modal)
      .layout(back, size)
      .layout(front, Size(4, 8))
    val s = Surface
      .blank(size)
      .blit(panes.rendered(back).render(None), Pos(0, 0), back)
      .blit(panes.rendered(front).render(None), Pos(2, 2), front)
    (panes, s)
  }

  val tests = Tests {

    test("a press focuses the pane it landed in and starts the drag there") {
      val (panes, s) = scene
      val p = panes.onPress(s.panes, Pos(3, 3))
      assert(p.focus == Some(front))
      assert(p.drag.map(_.pane) == Some(front))
    }

    test("the topmost pane wins where two overlap") {
      val (panes, s) = scene
      assert(panes.onPress(s.panes, Pos(3, 3)).drag.map(_.pane) == Some(front))
      assert(panes.onPress(s.panes, Pos(8, 3)).drag.map(_.pane) == Some(back))
    }

    test("a drag that wanders out of its pane keeps selecting in the pane it began in") {
      // Rule 6. The pointer ends up over the transcript, which is a real pane with a real
      // document -- the selection must still be the modal's, clamped to the modal's text.
      val (panes, s) = scene
      val dragged = panes.onPress(s.panes, Pos(2, 2)).onDrag(s.panes, Pos(9, 19))
      val d = dragged.drag.get
      assert(d.pane == front)
      val sel = d.selection
      assert(sel.end.entry < modal.doc.length)
      assert(modal.doc.clamp(sel.end) == sel.end)
      val (_, text) = dragged.onRelease
      assert(text == Some("yes\nno"))
    }

    test("a drag above its pane clamps to the pane's first row, not the screen's") {
      val (panes, s) = scene
      val d = panes.onPress(s.panes, Pos(4, 4)).onDrag(s.panes, Pos(0, 0)).drag.get
      assert(d.pane == front)
      assert(d.selection.start == grit.tui.model.select.DocPos(0, 0))
    }

    test("nothing under the pointer means no drag at all") {
      val panes = Panes.of(transcript).layout(back, size)
      val bare = Surface.blank(size)
      val p = panes.onPress(bare.panes, Pos(1, 1))
      assert(p.drag.isEmpty)
    }

    test("a drag with no press does nothing") {
      val (panes, s) = scene
      assert(panes.onDrag(s.panes, Pos(5, 5)).drag.isEmpty)
    }

    test("release yields the text of the pane's own document and ends the drag") {
      val (panes, s) = scene
      val (after, text) = panes.onPress(s.panes, Pos(3, 0)).onDrag(s.panes, Pos(3, 3)).onRelease
      assert(text == Some("del"))
      assert(after.drag.isEmpty)
    }

    test("an empty selection copies nothing") {
      val (panes, s) = scene
      val (_, text) = panes.onPress(s.panes, Pos(8, 0)).onRelease
      assert(text.isEmpty)
    }

    test("only the dragging pane is offered a selection to paint") {
      val (panes, s) = scene
      val p = panes.onPress(s.panes, Pos(3, 3)).onDrag(s.panes, Pos(3, 5))
      assert(p.selectionIn(front).isDefined)
      assert(p.selectionIn(back).isEmpty)
    }

    test("each pane keeps its own scroll position") {
      val panes = Panes.of(transcript, modal).layout(back, Size(2, 20)).layout(front, Size(2, 8))
      val scrolled = panes.scrollBy(back, -1)
      assert(scrolled.get(back).get.anchor != Anchor.Bottom)
      assert(scrolled.get(front).get.anchor == Anchor.Bottom)
      assert(scrolled.rendered(front).rows.map(_.text) == Vector("yes", "no"))
    }

    test("rendered is a total lookup: a pane never laid out reads empty") {
      assert(Panes.of(transcript).rendered(back) == Viewport.empty)
    }

    test("a wholesale document replacement does not inherit the wrapping it replaced") {
      // The /clear shape: entry indices restart at zero, so a fresh block carrying a
      // stale revision lands on a slot the old document wrapped. withDoc serves that
      // old wrapping -- the recorded per-entry-revision rule cannot say "nothing
      // survived" -- which is what resetDoc exists for.
      val panes = Panes.of(transcript).layout(back, size)
      val stale = panes.withDoc(back, Doc.of("x-ray")).layout(back, size)
      assert(stale.rendered(back).rows.map(_.text) == Vector("alpha")) // the trap
      val fresh = panes.resetDoc(back, Doc.of("x-ray")).layout(back, size)
      assert(fresh.rendered(back).rows.map(_.text) == Vector("x-ray"))
    }

    test("withDoc re-lays the pane out at the size it was last painted at") {
      // The old stale-frame mutation checks, promoted: a viewport describing an older
      // document is not something a caller can forget to refresh, because withDoc is the
      // only path content changes through and it re-wraps before returning.
      val laid = Panes.of(transcript).layout(back, size)
      val next = laid.withDoc(back, Doc.of("one two three four five six"))
      val (fresh, _) = next.get(back).get.viewport(size)
      assert(next.rendered(back).size == size)
      assert(next.rendered(back).rows == fresh.rows)
      // A pane never laid out has no size to re-wrap at, so it stays empty.
      assert(Panes.of(transcript).withDoc(back, Doc.of("x")).rendered(back) == Viewport.empty)
    }

    test("withAnchor re-lays the pane out at the size it was last painted at") {
      val laid = Panes.of(transcript).layout(back, size)
      val next = laid.withAnchor(back, Anchor.At(DocPos(1, 0)))
      val (fresh, _) = next.get(back).get.viewport(size)
      assert(next.rendered(back).rows == fresh.rows)
      assert(next.rendered(back).rows.map(_.text).take(2) == Vector("bravo", "charlie"))
    }

    test("scrollBy scrolls at the last painted size, with no size argument") {
      val laid = Panes.of(transcript).layout(back, size)
      val (expected, expPane) = laid.get(back).get.scrolledBy(-1, laid.rendered(back).size)
      val next = laid.scrollBy(back, -1)
      assert(next.rendered(back).rows == expected.rows)
      assert(next.get(back).get.anchor == expPane.anchor)
    }

    test("hit-testing inverts the stored viewport, not a fresh one") {
      // The document grew after the frame was painted -- the raw-copy path the mutators
      // closed, and exactly the case the recorded rule covers: between the frame and the
      // click the document may have grown, and what a press maps back through is what
      // was painted, never what a re-layout would now produce. The revision is bumped
      // with the text, as any real document change is, so the wrap cache cannot
      // accidentally make the fresh computation agree with the stored one.
      val laid = Panes.of(transcript).layout(back, size)
      val grownDoc = Doc(
        Vector(
          Block.Text("alpha and a very long line that wraps well past one row", 1L),
          Block.Text("bravo")
        )
      )
      val grown =
        laid.copy(panes =
          laid.panes.map(p =>
            if (p.id == back) { p.copy(doc = grownDoc) }
            else { p }
          )
        )
      val s = Surface.blank(size).blit(laid.rendered(back).render(None), Pos(0, 0), back)
      val pressed = grown.onPress(s.panes, Pos(1, 0))
      // Painted row 1 is entry 1 ("bravo"); a fresh layout of the grown document would
      // put the long first entry's continuation there instead.
      assert(pressed.drag.get.selection.start == DocPos(1, 0))
    }

    test("Panes.view paints the laid-out viewport with the drag's mask over it") {
      val (panes, s) = scene
      val dragging = panes.onPress(s.panes, Pos(3, 0)).onDrag(s.panes, Pos(3, 3))
      val painted = dragging.view(back).render(size)
      val expected = Surface
        .blank(size)
        .blit(dragging.rendered(back).render(dragging.selectionIn(back)), Pos(0, 0))
      assert(painted.lines == expected.lines)
      // And the mask on the painted surface is exactly the selection model's projection.
      val row0 = dragging.rendered(back).rows(0)
      val (from, to) = dragging.selectionIn(back).get.columnsOn(row0.entry, row0.start, row0.text)
      val reversed = (0 until size.cols).filter(c => painted.at(0, c).style.reverse)
      assert(reversed == (from until to))
    }

    test("Panes.view records its placement under the pane's own id") {
      val panes = Panes.of(transcript).layout(back, size)
      val s = panes.view(back).render(size)
      assert(Placements(s.panes)(back).contains(Rect(0, 0, size.rows, size.cols)))
      // A pane never laid out paints nothing and is placed nowhere.
      val bare = Panes.of(transcript).view(back).render(size)
      assert(Placements(bare.panes)(back).isEmpty)
      assert(bare.lines.forall(_.trim.isEmpty))
    }
  }
}
