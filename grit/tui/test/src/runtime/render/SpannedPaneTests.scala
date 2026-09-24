package grit.tui.runtime.render

import grit.tui.components.pane.{Anchor, Viewport}
import grit.tui.model.block.Block
import grit.tui.model.select.{Doc, DocPos, Selection}
import grit.tui.model.surface.{Color, Size, Style}
import grit.tui.model.text.{StyledText, Width}

import utest.*

/** Rule 5, restated for colour.
  *
  * A span and the selection over it are two masks laid on the same cells, and they get
  * their columns from the same projection ([[Width.columnAtOffset]]). If they ever stop
  * doing so, a span drifts a column away from the highlight on top of it -- invisible to
  * anything that inspects the model or the clipboard, which is precisely the family of
  * bug FINDINGS records.
  */
object SpannedPaneTests extends TestSuite {

  private val Ink = Color.hex("#c0caf5")
  private val Iris = Color.hex("#bb9af7")
  private val Slab = Color.hex("#292e42")

  private def paneOf(doc: Doc, size: Size): Viewport = {
    val m = DocMemo.empty.synced(doc, size.cols)
    m.viewport(m.topFor(Anchor.Bottom, size.rows), size)
  }

  /** One block: a five-character marker in iris, the rest in ink, on a tinted ground. */
  private def marked(marker: String, body: String): Block.Text =
    Block
      .styled(
        StyledText.styled(marker, Style.fg(Iris) + Style.Bold) ++
          StyledText.styled(body, Style.fg(Ink))
      )
      .copy(ground = Style.bg(Slab))

  val tests = Tests {

    test("a span paints on exactly the cells its offsets name") {
      val doc = Doc(Vector(marked("you> ", "hello")))
      val s = paneOf(doc, Size(1, 20)).render(None)
      (0 until 5).foreach { c => assert(s.at(0, c).style.fg.contains(Iris)) }
      (5 until 10).foreach { c => assert(s.at(0, c).style.fg.contains(Ink)) }
      assert(s.at(0, 4).style.bold && !s.at(0, 5).style.bold)
    }

    test("a block's ground runs the full width of the row, past the end of its text") {
      // The reason ground is not just a span over the text: a short line would leave the
      // slab ragged at the right, and the tint is what separates one turn from the next.
      val doc = Doc(Vector(marked("you> ", "hi")))
      val s = paneOf(doc, Size(1, 20)).render(None)
      (0 until 20).foreach { c => assert(s.at(0, c).style.bg.contains(Slab)) }
    }

    test("a selection reverses exactly the cells the model names, over spanned text") {
      // The property the whole thing turns on. The highlight is a mask laid over spans,
      // not a style threaded through them, so a coloured transcript highlights with no
      // change to the selection code at all.
      val doc = Doc(Vector(marked("you> ", "hello there")))
      val vp = paneOf(doc, Size(1, 20))
      val sel = Selection(DocPos(0, 3), DocPos(0, 9))
      val s = vp.render(Some(sel))
      (0 until 16).foreach { c =>
        val inside = c >= 3 && c < 9
        assert(s.at(0, c).style.reverse == inside)
      }
    }

    test("the colours under a selection are untouched by it") {
      // Reverse swaps whatever fg and bg the cell carries, so the highlight must add the
      // attribute and change nothing else. A mask that overwrote the colour would look
      // right on a monochrome grid and wrong here.
      val doc = Doc(Vector(marked("you> ", "hello there")))
      val vp = paneOf(doc, Size(1, 20))
      val plain = vp.render(None)
      val marked2 = vp.render(Some(Selection(DocPos(0, 0), DocPos(0, 16))))
      (0 until 20).foreach { c =>
        assert(marked2.at(0, c).ch == plain.at(0, c).ch)
        assert(marked2.at(0, c).style.fg == plain.at(0, c).style.fg)
        assert(marked2.at(0, c).style.bg == plain.at(0, c).style.bg)
        assert(marked2.at(0, c).style.bold == plain.at(0, c).style.bold)
      }
    }

    test("a span and the selection agree about where a wide glyph put them") {
      // Both go through Width.columnAtOffset. A CJK glyph before either one pushes it two
      // cells, and if only one of them knew that they would land a column apart.
      val text = "你好x" + "abcdef"
      val doc = Doc(
        Vector(Block.styled(StyledText.styled(text, Style.fg(Iris)).under(Style.plain)))
      )
      val vp = paneOf(doc, Size(1, 30))
      // The span covers the whole text, so its painted extent is the text's own width.
      val painted = vp.render(None)
      val width = Width.of(text)
      (0 until width).foreach { c => assert(painted.at(0, c).style.fg.contains(Iris)) }
      assert(!painted.at(0, width).style.fg.contains(Iris))
      // ...and a selection to the same offset stops at the same column.
      val s = vp.render(Some(Selection(DocPos(0, 0), DocPos(0, text.length))))
      (0 until width).foreach { c => assert(s.at(0, c).style.reverse) }
      assert(!s.at(0, width).style.reverse)
    }

    test("a span is clipped onto every row a wrapped block breaks into") {
      // One span over a whole paragraph, cut by the wrapper into as many pieces as there
      // are rows -- and every row still carries it, because rebasing happens per row.
      val body = ("word " * 30).trim
      val doc = Doc(Vector(Block.styled(StyledText.styled(body, Style.fg(Ink)))))
      val vp = paneOf(doc, Size(6, 20))
      assert(vp.rows.length > 3)
      vp.rows.indices.foreach { r =>
        val row = vp.rows(r)
        assert(row.spans.nonEmpty)
        assert(row.spans.head.from == 0)
        assert(row.spans.head.to == row.text.length)
      }
    }

    test("a diff's tint is per line and runs the full width") {
      val doc = Doc(
        Vector(
          Block.Diff(
            Vector("+ added", "- removed", "  context"),
            style = Block.DiffStyle(
              added = Style.bg(Color.hex("#1e2b22")),
              removed = Style.bg(Color.hex("#2b1e24")),
              context = Style.plain
            )
          )
        )
      )
      val s = paneOf(doc, Size(3, 20)).render(None)
      (0 until 20).foreach { c =>
        assert(s.at(0, c).style.bg.contains(Color.hex("#1e2b22")))
        assert(s.at(1, c).style.bg.contains(Color.hex("#2b1e24")))
        assert(s.at(2, c).style.bg.isEmpty)
      }
    }

    test("an undressed block paints exactly as it did before colour existed") {
      // The whole library default: grit.tui ships no palette, so a document nobody dressed
      // is a monochrome document.
      val doc = Doc.of("plain text", "more of it")
      val s = paneOf(doc, Size(2, 20)).render(None)
      (0 until 2).foreach { r =>
        (0 until 20).foreach { c => assert(s.at(r, c).style == Style.plain) }
      }
    }
  }
}
