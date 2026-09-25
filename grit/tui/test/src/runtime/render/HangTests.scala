package grit.tui.runtime.render

import grit.tui.components.pane.{Anchor, Viewport}
import grit.tui.model.block.{Block, Overflow}
import grit.tui.model.select.{Doc, DocPos, Selection}
import grit.tui.model.surface.{Color, Pos, Size, Style}
import grit.tui.model.text.{StyledText, Width}

import utest.*

/** A block's lead and hang: painted beside its rows, never part of its text. Read off the
  * painted surface, the wrap memo and the viewport's inverse, since a margin that is
  * painted right but indexed wrong copies the rail or loses a character.
  */
object HangTests extends TestSuite {

  private val Rail = Color.hex("#565f89")

  private def paneOf(doc: Doc, size: Size): Viewport = {
    val m = DocMemo.empty.synced(doc, size.cols)
    m.viewport(m.topFor(Anchor.At(DocPos.zero), size.rows), size)
  }

  private val rail = StyledText.styled("▎ ", Style.fg(Rail))

  val tests = Tests {

    test("a lead and a hang are painted beside the rows, and the text wraps in what is left") {
      val b = Block
        .Text("alpha beta gamma delta")
        .beside(StyledText("▌ᚨ "), rail)
      val vp = paneOf(Doc(Vector(b)), Size(4, 14))
      val s = vp.render(None)
      s.lines.map(_.stripTrailing) ==> Vector("▌ᚨ alpha beta", "▎ gamma delta", "", "")
      assert(s.at(1, 0).style.fg.contains(Rail))
      // The text never includes the margin.
      vp.rows.map(_.text) ==> Vector("alpha beta", "gamma delta")
    }

    test("a position in a margin is its row's first; past it, the text's own") {
      val b = Block.Text("alpha beta gamma delta").beside(StyledText("▌ᚨ "), rail)
      val vp = paneOf(Doc(Vector(b)), Size(4, 14))
      vp.docPosAt(Pos(0, 0)) ==> Some(DocPos(0, 0))
      vp.docPosAt(Pos(0, 2)) ==> Some(DocPos(0, 0))
      vp.docPosAt(Pos(0, 4)) ==> Some(DocPos(0, 1))
      vp.docPosAt(Pos(1, 1)) ==> Some(DocPos(0, 11))
      vp.docPosAt(Pos(1, 3)) ==> Some(DocPos(0, 12))
    }

    test("a selection reverses the text alone: the margin is never highlighted") {
      val b = Block.Text("alpha beta gamma delta").beside(StyledText("▌ᚨ "), rail)
      val doc = Doc(Vector(b))
      val vp = paneOf(doc, Size(2, 14))
      val sel = Selection(DocPos(0, 6), DocPos(0, 16))
      val s = vp.render(Some(sel))
      val shown = (0 until 2).toVector.map(r =>
        (0 until 14).filter(c => s.at(r, c).style.reverse).map(s.at(r, _).ch).mkString
      )
      shown ==> Vector("beta", "gamma")
      doc.textOf(sel) ==> "beta gamma"
      assert(!s.at(1, 0).style.reverse && !s.at(1, 1).style.reverse)
    }

    test("a truncating block keeps its hang on every line and cuts after it") {
      val b = Block
        .Text("0123456789\nabcdefghij", overflow = Overflow.Truncate)
        .beside(StyledText("  "), StyledText("  "))
      val s = paneOf(Doc(Vector(b)), Size(2, 8)).render(None)
      s.lines ==> Vector("  012345", "  abcdef")
    }

    test("a block's width is its text's own: the memo re-wraps when the margin changes") {
      val plain = Block.Text("one two three")
      val hung = plain.beside(StyledText("> "), StyledText("> "))
      val m1 = DocMemo.empty.synced(Doc(Vector(plain)), 8)
      val m2 = m1.synced(Doc(Vector(hung)), 8)
      m1.rows(0).map(_.text) ==> Vector("one two", "three")
      m2.rows(0).map(_.text) ==> Vector("one", "two", "three")
      assert(m2.misses == m1.misses + 1)
      assert(Width.of(hung.lead.text) == 2)
    }
  }
}
