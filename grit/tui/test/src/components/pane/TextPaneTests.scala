package grit.tui.components.pane

import grit.tui.model.block.Block
import grit.tui.model.select.{Doc, DocPos, Selection}
import grit.tui.model.surface.{PaneId, Pos, Size}
import grit.tui.model.text.{Width, WrapCache}
import utest.*

/** The scroll model, checked against the three properties the design rests on: the
  * reading position survives a re-wrap, wrapping costs a viewport rather than a
  * document, and `docPosAt` really is the inverse of what was painted.
  */
object TextPaneTests extends TestSuite {

  private val id = PaneId.of("transcript")

  /** A document of `n` entries, each long enough to wrap several times at 40 columns. */
  private def long(n: Int): Doc =
    Doc((0 until n).toVector.map { i =>
      Block.Text(s"entry $i " + (0 until 30).map(w => s"word$w").mkString(" "))
    })

  private def pane(doc: Doc, width: Int = 40): TextPane =
    TextPane(id, doc, cache = WrapCache.empty(width))

  val tests = Tests {

    test("a bottom-anchored pane shows the tail of the document") {
      val (vp, _) = pane(Doc.of("a", "b", "c", "d", "e")).viewport(Size(3, 20))
      assert(vp.rows.map(_.text) == Vector("c", "d", "e"))
    }

    test("a document shorter than the screen paints from the top row down") {
      // No blank rows above it: a transcript that has only just started reads from the
      // top, and only pins to the bottom once it overflows.
      val (vp, _) = pane(Doc.of("a", "b")).viewport(Size(6, 20))
      assert(vp.rows.map(_.text) == Vector("a", "b"))
    }

    test("an anchored pane starts at the row holding the anchor") {
      val doc = Doc.of("aaa", "bbb", "ccc", "ddd")
      val p = pane(doc).withAnchor(Anchor.At(DocPos(1, 0)))
      val (vp, _) = p.viewport(Size(2, 20))
      assert(vp.rows.map(_.text) == Vector("bbb", "ccc"))
    }

    test("the reading position survives a re-wrap") {
      // The property that forced DocPos: an anchor names a place in the logical text, so
      // narrowing the window re-breaks the rows underneath the reader without moving the
      // reader. An anchor stored as a wrapped row index cannot do this.
      val doc = long(20)
      val at = DocPos(9, 200)
      val p = pane(doc, 80).withAnchor(Anchor.At(at))
      val (wide, p1) = p.viewport(Size(10, 80))
      val (narrow, _) = p1.viewport(Size(10, 34))
      def holds(vp: Viewport): Boolean = vp.rows.headOption.exists { r =>
        r.entry == at.entry && r.start <= at.offset && at.offset <= r.start + r.text.length
      }
      // The anchored row is not the entry's first row at either width, so an anchor kept
      // as a row index would have to land somewhere else.
      assert(wide.rows.head.start > 0)
      assert(narrow.rows.head.start > 0)
      assert(wide.rows.head.start != narrow.rows.head.start)
      assert(holds(wide))
      assert(holds(narrow))
    }

    test("wrapping costs a viewport, not a document") {
      // 400 entries, ten rows on screen: the walk stops as soon as it has filled the
      // viewport, so misses are bounded by the rows shown and not by the entries held.
      val (vp, p) = pane(long(400)).viewport(Size(10, 40))
      assert(vp.rows.length == 10)
      assert(p.cache.misses <= 12)
    }

    test("appending to a document re-wraps only the entry that changed") {
      val doc = long(30)
      val (_, p) = pane(doc).viewport(Size(10, 40))
      val before = p.cache.misses
      val grown =
        doc.copy(blocks = doc.blocks.updated(29, Block.Text(doc.textAt(29) + " more", 1L)))
      val (_, p2) = p.withDoc(grown).viewport(Size(10, 40))
      assert(p2.cache.misses == before + 1)
      assert(p2.cache.hits > p.cache.hits)
    }

    test("a width change discards the wrapped rows, because every one of them is wrong") {
      val (_, p) = pane(long(30)).viewport(Size(10, 40))
      val (_, p2) = p.viewport(Size(10, 20))
      assert(p2.cache.width == 20)
      assert(p2.cache.misses > p.cache.misses)
    }

    test("scrolling back a screen and forward again lands on the same rows") {
      val size = Size(8, 40)
      val (start, p) = pane(long(20)).viewport(size)
      val (back, p1) = p.scrolledBy(-8, size)
      val (there, _) = p1.scrolledBy(8, size)
      assert(back.rows != start.rows)
      assert(there.rows == start.rows)
    }

    test("scrolling past the end pins back to the bottom") {
      val size = Size(8, 40)
      val (bottom, p0) = pane(long(20)).viewport(size)
      val (_, p1) = p0.scrolledBy(-20, size)
      val (vp, p2) = p1.scrolledBy(500, size)
      assert(p2.anchor == Anchor.Bottom)
      assert(vp.rows == bottom.rows)
    }

    test("scrolling up stops at the first row of the document") {
      val size = Size(8, 40)
      val (_, p) = pane(long(20)).viewport(size)
      val (vp, _) = p.scrolledBy(-9999, size)
      val head = vp.rows.head
      assert(head.entry == 0)
      assert(head.start == 0)
    }

    test("docPosAt inverts render for every painted cell") {
      // The real inverse: the glyph the painter put in a cell must be the glyph living
      // at the document position that cell maps back to. Hit-testing and copying are the
      // same function read in opposite directions.
      val doc = long(12)
      val size = Size(10, 40)
      val (vp, _) = pane(doc).viewport(size)
      val painted = vp.render(None)
      var checked = 0
      vp.rows.zipWithIndex.foreach { (row, r) =>
        (0 until Width.of(row.text)).foreach { c =>
          vp.docPosAt(Pos(r, c)) match {
            case Some(dp) =>
              val text = doc.textAt(dp.entry)
              assert(dp.offset < text.length)
              assert(text.charAt(dp.offset) == painted.at(r, c).ch)
              checked += 1
            case None => assert(false)
          }
        }
      }
      assert(checked > 200)
    }

    test("a click below the last row selects to the end of the document") {
      val doc = Doc.of("aa", "bb")
      val (vp, _) = pane(doc).viewport(Size(8, 20))
      assert(vp.docPosAt(Pos(6, 0)) == Some(doc.end))
    }

    test("a click outside the viewport maps nowhere") {
      val (vp, _) = pane(Doc.of("aa")).viewport(Size(4, 20))
      assert(vp.docPosAt(Pos(-1, 0)).isEmpty)
      assert(vp.docPosAt(Pos(4, 0)).isEmpty)
      assert(vp.docPosAt(Pos(0, 20)).isEmpty)
    }

    test("the highlighted cells are exactly what the selection model says") {
      // Rule 5, made visible: the painted mask is read back off the surface and compared
      // with the model row by row. An asymmetric projection paints rows after the
      // selection and leaves the copied text correct -- only this catches it.
      val doc = long(6)
      val size = Size(10, 40)
      val (vp, _) = pane(doc).withAnchor(Anchor.At(DocPos(0, 0))).viewport(size)
      val sel = Selection(DocPos(1, 4), DocPos(3, 9))
      val painted = vp.render(Some(sel))
      vp.rows.zipWithIndex.foreach { (row, r) =>
        val (from, to) = sel.columnsOn(row.entry, row.start, row.text)
        val reversed = (0 until size.cols).filter(c => painted.at(r, c).style.reverse)
        assert(reversed == (from until to))
      }
    }

    test("an empty document renders nothing and maps nowhere useful") {
      val (vp, _) = pane(Doc.empty).viewport(Size(5, 20))
      assert(vp.rows.isEmpty)
      assert(vp.docPosAt(Pos(0, 0)) == Some(DocPos.zero))
      assert(vp.render(None).lines.forall(_.trim.isEmpty))
    }
  }
}
