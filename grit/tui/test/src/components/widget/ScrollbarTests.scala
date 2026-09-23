package grit.tui.components.widget

import grit.tui.components.pane.{Panes, TextPane}
import grit.tui.model.select.Doc
import grit.tui.model.text.WrapCache
import grit.tui.wire.paint.Painter
import grit.tui.model.surface.{Frame, Hit, PaneId, Pos, Size, Surface}
import utest.*

object ScrollbarTests extends TestSuite {

  /** A 20-row track for a document of 100 rows showing 10. */
  private val sb = Scrollbar(content = 100, window = 10, offset = 0)

  val tests = Tests {

    test("the thumb is proportional and pinned to the ends of its travel") {
      assert(sb.thumb(20) == (0, 2)) // 20 rows * (10/100) = 2
      assert(sb.copy(offset = 45).thumb(20) == (9, 2))
      assert(sb.copy(offset = 90).thumb(20) == (18, 2)) // bottom of the track
    }

    test("the thumb never disappears and never exceeds the track") {
      // A needle in a huge document still gets one row.
      assert(Scrollbar(10000, 1, 0).thumb(5) == (0, 1))
      assert(Scrollbar(10000, 1, 9999).thumb(5) == (4, 1))
      // A window most of the document: clamped, not inverted.
      assert(Scrollbar(5, 1, 0).thumb(3) == (0, 1))
    }

    test("a document that fits is all thumb") {
      val s = Scrollbar(10, 20, 3).render(Size(6, 1))
      assert(Scrollbar(10, 20, 3).thumb(6) == (0, 6))
      assert(s.lines.forall(_ == "█"))
    }

    test("the offset is clamped into the document") {
      assert(sb.copy(offset = 500).thumb(20) == (18, 2))
      assert(sb.copy(offset = -5).thumb(20) == (0, 2))
    }

    test("the track renders as rail with a thumb block, one column") {
      val s = sb.copy(offset = 45).render(Size(20, 1))
      assert(s.size == Size(20, 1))
      val rows = s.lines
      assert(rows.count(_ == "█") == 2)
      assert(rows.count(_ == "│") == 18)
      assert(rows(9) == "█" && rows(10) == "█")
    }

    test("dragging maps a track row to an offset: endpoints exact, monotone, within one notch") {
      val s = sb.copy(offset = 0)
      // Track 20, thumb 2: travel 18 for a span of 90, so a notch is 5 rows of document.
      assert(s.offsetAtRow(20, 0) == 0)
      assert(s.offsetAtRow(20, 9) == 45)
      assert(s.offsetAtRow(20, 18) == 90)
      assert(s.offsetAtRow(20, 19) == 90) // grabbed past the travel: clamped, not wrapped
      // Monotone, and dragging from where the thumb sits cannot jump more than a notch.
      val s2 = Scrollbar(97, 11, 0)
      val notch = math.ceil(86.0 / 12).toInt
      var prev = 0
      var o = 0
      while (o <= 86) {
        val (start, _) = s2.copy(offset = o).thumb(13)
        val back = s2.offsetAtRow(13, start)
        assert(back <= o)
        assert(o - back < notch)
        assert(back >= prev)
        prev = back
        o += 7
      }
    }

    test("the draggable thumb is the placement map, end to end") {
      // The §8 answer, demonstrated: blit under a PaneId, hit-test the frame, map the
      // local position through the widget's geometry. No widget framework appears.
      val root = Surface
        .blank(Size(24, 80))
        .blit(sb.copy(offset = 45).render(Size(19, 1)), Pos(1, 79), PaneId.of("scrollbar"))
      val hit = Hit.paneAt(root.panes, Pos(5, 79))
      assert(hit.isDefined)
      val (id, local) = hit.get
      assert(id == PaneId.of("scrollbar"))
      assert(local == Pos(4, 0))
      // Track 19, thumb 1, travel 18, span 90: row 4 of the track is offset 20.
      assert(sb.copy(offset = 45).offsetAtRow(19, local.row) == 20)
    }

    test("of reads the pane's own funnel-maintained index") {
      // The wiring the demo used to hand-maintain: content is the pane's whole wrapped
      // document, offset the row its viewport reads from -- both read back off the pane,
      // none computed here.
      val id = PaneId.of("transcript")
      val panes = Panes
        .of(TextPane(id, Doc.of("alpha", "bravo", "charlie", "delta"), cache = WrapCache.empty(20)))
        .layout(id, Size(2, 20))
      val index = panes.rowIndex(id)
      val scrolled = panes.scrollBy(id, -1)
      val sb = Scrollbar.of(scrolled, id, window = 2)
      assert(sb.content == index.length)
      assert(sb.offset == scrolled.rendered(id).top.flatMap(index.indexOf).get)
      // A pane never laid out reads the honest zero geometry.
      val bare = Panes.of(TextPane(id, Doc.empty))
      assert(Scrollbar.of(bare, id, 5) == Scrollbar(0, 5, 0))
    }

    test("a scrollbar whose geometry did not change writes zero bytes") {
      // Different document, same proportions: the thumb did not move, the painter
      // must not be fed a reason to.
      val f1 = Frame(Scrollbar(100, 10, 45).render(Size(20, 1)))
      val f2 = Frame(Scrollbar(200, 20, 90).render(Size(20, 1)))
      assert(f1.surface == f2.surface)
      assert(Painter.paint(f2, Some(f1)) == "")
    }
  }
}
