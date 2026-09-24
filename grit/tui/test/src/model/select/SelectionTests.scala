package grit.tui.model.select

import grit.tui.model.surface.*
import grit.tui.model.text.{Width, Wrap}

import utest.*

object SelectionTests extends TestSuite {

  private val doc = Doc.of(
    "the signature is the whole truth about a function",
    "effects are data",
    "a drag belongs to the pane it began in"
  )

  /** Paint `doc` wrapped to `width` onto a fresh surface, one entry after another, and
    * apply `sel` as a reverse mask through the same projection the app would use.
    *
    * Returns the surface and, for every painted cell, the [[DocPos]] it shows -- so the
    * mask can be compared against the model cell by cell.
    */
  private def painted(sel: Selection, width: Int): (Surface, Map[Pos, DocPos]) = {
    var s = Surface.blank(Size(24, width + 1))
    val where = Map.newBuilder[Pos, DocPos]
    var row = 0
    var entry = 0
    while (entry < doc.length) {
      val rows = Wrap.wrap(doc.textAt(entry), width)
      var i = 0
      while (i < rows.length) {
        val r = rows(i)
        s = s.write(row, 0, r.text)
        var off = 0
        while (off < r.text.length) {
          where += (Pos(row, Width.columnAtOffset(r.text, off)) -> DocPos(
            entry,
            r.startOffset + off
          ))
          off += 1
        }
        val (from, to) = sel.columnsOn(entry, r.startOffset, r.text)
        if (to > from) {
          s = s.restyle(Rect(row, from, 1, to - from), st => st.copy(reverse = true))
        }
        row += 1
        i += 1
      }
      entry += 1
    }
    (s, where.result())
  }

  /** The oracle, and the whole reason this package exists (FINDINGS bug 3): the
    * reverse-video cells must equal what the selection model says, for every cell of
    * every row. The copied text stayed correct while this was broken next door, so
    * nothing short of sweeping the painted grid can see it.
    */
  private def assertMaskMatchesModel(sel: Selection, width: Int): Unit = {
    val (s, where) = painted(sel, width)
    var row = 0
    while (row < s.size.rows) {
      var col = 0
      while (col < s.size.cols) {
        val reverse = s.at(row, col).style.reverse
        val expected = where.get(Pos(row, col)).exists(sel.contains)
        assert(reverse == expected)
        col += 1
      }
      row += 1
    }
  }

  val tests = Tests {

    test("both bounds project onto a row identically, so a row past the selection is clear") {
      // The asymmetric version mapped a bound before the row to 0 for start and to the
      // row's length for end: a row entirely past the selection got the whole row.
      val sel = Selection(DocPos(1, 2), DocPos(1, 6))
      assert(sel.columnsOn(0, 0, "the signature") == (13, 13)) // entirely before
      assert(sel.columnsOn(2, 0, "a drag belongs") == (0, 0)) // entirely after
      assert(sel.columnsOn(1, 0, "effects are data") == (2, 6))
    }

    test("a fresh click selects nothing and lights up nothing") {
      // The regression next door: an empty selection painted every row below it.
      val sel = Selection(DocPos(1, 3), DocPos(1, 3))
      assert(sel.isEmpty)
      assertMaskMatchesModel(sel, 20)
    }

    test("the mask equals the model for a selection inside one entry") {
      assertMaskMatchesModel(Selection(DocPos(0, 4), DocPos(0, 21)), 20)
    }

    test("the mask equals the model for a selection spanning entries and wrapped rows") {
      assertMaskMatchesModel(Selection(DocPos(0, 30), DocPos(2, 14)), 20)
    }

    test("the mask equals the model at a width that forces many rows") {
      assertMaskMatchesModel(Selection(DocPos(0, 2), DocPos(2, 3)), 8)
    }

    test("projection is measured in display columns, not char offsets") {
      // A wide glyph before the selection pushes it right by two cells, not one.
      val sel = Selection(DocPos(0, 2), DocPos(0, 4))
      assert(sel.columnsOn(0, 0, "世界ab") == (4, 6))
    }

    test("a selection is normalised however the drag ran") {
      val forward = Selection.between(DocPos(0, 2), DocPos(1, 5))
      val backward = Selection.between(DocPos(1, 5), DocPos(0, 2))
      assert(forward == backward)
      assert(forward.start == DocPos(0, 2))
    }

    test("copied text is a slice of the logical document and carries no wrap breaks") {
      // The point of DocPos indexing unwrapped text: the clipboard gets what the author
      // wrote, not what a narrow viewport broke it to.
      val sel = Selection(DocPos(0, 4), DocPos(1, 7))
      val text = doc.textOf(sel)
      assert(text == "signature is the whole truth about a function\neffects")
      assert(doc.textOf(Selection(DocPos(0, 4), DocPos(0, 13))) == "signature")
    }

    test("an empty selection copies nothing") {
      assert(doc.textOf(Selection(DocPos(1, 3), DocPos(1, 3))) == "")
    }

    test("out-of-range positions are clamped into the document, never thrown") {
      assert(doc.clamp(DocPos(-1, -5)) == DocPos(0, 0))
      assert(doc.clamp(DocPos(99, 99)) == DocPos(2, doc.textAt(2).length))
      assert(doc.clamp(DocPos(1, 99)) == DocPos(1, doc.textAt(1).length))
      assert(
        doc.textOf(Selection(DocPos(-3, -3), DocPos(99, 99))) == doc.blocks
          .map(_.text)
          .mkString("\n")
      )
    }
  }
}
