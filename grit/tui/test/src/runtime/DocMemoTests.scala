package grit.tui.runtime

import grit.tui.components.pane.{Anchor, Viewport}
import grit.tui.model.block.Block
import grit.tui.model.select.{Doc, DocPos, Selection}
import grit.tui.model.surface.{Pos, Size}
import grit.tui.model.text.Width
import utest.*

/** The scroll model, checked against the properties the design rests on: the reading
  * position survives a re-wrap, only a changed block re-wraps, and `docPosAt` really is
  * the inverse of what was painted.
  */
object DocMemoTests extends TestSuite {

  /** A document of `n` entries, each long enough to wrap several times at 40 columns. */
  private def long(n: Int): Doc =
    Doc((0 until n).toVector.map { i =>
      Block.Text(s"entry $i " + (0 until 30).map(w => s"word$w").mkString(" "))
    })

  /** `doc` at `size`, read from `anchor`: the viewport, its top row, and the memo. */
  private def view(
      doc: Doc,
      size: Size,
      anchor: Anchor = Anchor.Bottom,
      memo: DocMemo = DocMemo.empty
  ): (Viewport, Int, DocMemo) = {
    val m = memo.synced(doc, size.cols)
    val top = m.topFor(anchor, size.rows)
    (m.viewport(top, size), top, m)
  }

  val tests = Tests {

    test("a bottom-anchored pane shows the tail of the document") {
      val (vp, _, _) = view(Doc.of("a", "b", "c", "d", "e"), Size(3, 20))
      assert(vp.rows.map(_.text) == Vector("c", "d", "e"))
    }

    test("a document shorter than the screen paints from the top row down") {
      // No blank rows above it: a transcript that has only just started reads from the
      // top, and only pins to the bottom once it overflows.
      val (vp, _, _) = view(Doc.of("a", "b"), Size(6, 20))
      assert(vp.rows.map(_.text) == Vector("a", "b"))
    }

    test("an anchored pane starts at the row holding the anchor") {
      val (vp, _, _) =
        view(Doc.of("aaa", "bbb", "ccc", "ddd"), Size(2, 20), Anchor.At(DocPos(1, 0)))
      assert(vp.rows.map(_.text) == Vector("bbb", "ccc"))
    }

    test("the reading position survives a re-wrap") {
      // The property that forced DocPos: an anchor names a place in the logical text, so
      // narrowing the window re-breaks the rows underneath the reader without moving the
      // reader. An anchor stored as a wrapped row index cannot do this.
      val doc = long(20)
      val at = DocPos(9, 200)
      val (wide, _, m) = view(doc, Size(10, 80), Anchor.At(at))
      val (narrow, _, _) = view(doc, Size(10, 34), Anchor.At(at), m)
      def holds(vp: Viewport): Boolean = vp.rows.headOption.exists { r =>
        r.entry == at.entry && r.start <= at.offset && at.offset <= r.start + r.text.length
      }
      // The anchored row is not the entry's first row at either width, so an anchor kept
      // as a row index would have to land somewhere else.
      assert(wide.rows.head.start > 0, narrow.rows.head.start > 0)
      assert(wide.rows.head.start != narrow.rows.head.start)
      assert(holds(wide), holds(narrow))
    }

    test("changing one block re-wraps that block alone") {
      val doc = long(30)
      val (_, _, m) = view(doc, Size(10, 40))
      val grown = doc.updated(29, Block.Text(doc.textAt(29) + " more"))
      val (_, _, m2) = view(grown, Size(10, 40), memo = m)
      assert(m2.misses == m.misses + 1, m2.hits == m.hits + 29)
    }

    test("a width change discards the wrapped rows, because every one of them is wrong") {
      val (_, _, m) = view(long(30), Size(10, 40))
      val (_, _, m2) = view(long(30), Size(10, 20), memo = m)
      assert(m2.width == 20, m2.misses == m.misses + 30)
    }

    test("scrolling back a screen and forward again lands on the same rows") {
      val size = Size(8, 40)
      val (start, top, m) = view(long(20), size)
      val (back, backTop, _) = view(long(20), size, m.scrolled(top, -8, size.rows), m)
      val (there, _, _) = view(long(20), size, m.scrolled(backTop, 8, size.rows), m)
      assert(back.rows != start.rows, there.rows == start.rows)
    }

    test("scrolling past the end pins back to the bottom") {
      val size = Size(8, 40)
      val (_, top, m) = view(long(20), size)
      assert(m.scrolled(top - 20, 500, size.rows) == Anchor.Bottom)
    }

    test("scrolling up stops at the first row of the document") {
      val size = Size(8, 40)
      val (_, top, m) = view(long(20), size)
      val (vp, _, _) = view(long(20), size, m.scrolled(top, -9999, size.rows), m)
      assert(vp.rows.head.entry == 0, vp.rows.head.start == 0)
    }

    test("docPosAt inverts render for every painted cell") {
      // The real inverse: the glyph the painter put in a cell must be the glyph living
      // at the document position that cell maps back to. Hit-testing and copying are the
      // same function read in opposite directions.
      val doc = long(12)
      val (vp, _, _) = view(doc, Size(10, 40))
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
      val (vp, _, _) = view(doc, Size(8, 20))
      assert(vp.docPosAt(Pos(6, 0)) == Some(doc.end))
    }

    test("a click outside the viewport maps nowhere") {
      val (vp, _, _) = view(Doc.of("aa"), Size(4, 20))
      assert(vp.docPosAt(Pos(-1, 0)).isEmpty, vp.docPosAt(Pos(4, 0)).isEmpty)
      assert(vp.docPosAt(Pos(0, 20)).isEmpty)
    }

    test("the highlighted cells are exactly what the selection model says") {
      // Rule 5, made visible: the painted mask is read back off the surface and compared
      // with the model row by row. An asymmetric projection paints rows after the
      // selection and leaves the copied text correct -- only this catches it.
      val size = Size(10, 40)
      val (vp, _, _) = view(long(6), size, Anchor.At(DocPos(0, 0)))
      val sel = Selection(DocPos(1, 4), DocPos(3, 9))
      val painted = vp.render(Some(sel))
      vp.rows.zipWithIndex.foreach { (row, r) =>
        val (from, to) = sel.columnsOn(row.entry, row.start, row.text)
        val reversed = (0 until size.cols).filter(c => painted.at(r, c).style.reverse)
        assert(reversed == (from until to))
      }
    }

    test("an empty document renders nothing and maps nowhere useful") {
      val (vp, _, _) = view(Doc.empty, Size(5, 20))
      assert(vp.rows.isEmpty, vp.docPosAt(Pos(0, 0)) == Some(DocPos.zero))
      assert(vp.render(None).lines.forall(_.trim.isEmpty))
    }
  }
}
