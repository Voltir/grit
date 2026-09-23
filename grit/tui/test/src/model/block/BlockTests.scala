package grit.tui.model.block

import grit.tui.components.pane.TextPane
import grit.tui.model.select.{Doc, DocPos, Selection}
import grit.tui.model.surface.{PaneId, Pos, Size}
import grit.tui.model.text.WrapCache
import utest.*

/** Transcript blocks that are not one string of text.
  *
  * The contracts that matter:
  *
  *   - a block's logical text is the rows it measures itself into, joined by newlines --
  *     `DocPos` offsets index it exactly as they indexed a plain entry, so the whole
  *     selection model survives unchanged;
  *   - a tool call is a *group of rows inside one block*, so expanding it moves no
  *     sibling position and invalidates no sibling cache key -- the property an
  *     entry-per-line flattening would have broken;
  *   - truncation is display-only: what is copied is always the whole logical line;
  *   - a separator paints a rule across the pane's width but copies as an empty line.
  */
object BlockTests extends TestSuite {

  private val id = PaneId.of("transcript")

  private def pane(doc: Doc, width: Int): TextPane =
    TextPane(id, doc, cache = WrapCache.empty(width))

  val tests = Tests {

    test("a tool call is one summary line, and the glyph tells the state") {
      val running = Block.tool("Read", "src/Main.scala", tick = 3)
      assert(running.text == "⠸ Read(src/Main.scala)")
      assert(running.state == ToolState.Running(3))

      val done = running.finish(ok = true, "3 files, 42 lines")
      assert(done.text == "✓ Read(src/Main.scala) · 3 files, 42 lines")

      val failed = Block.tool("Bash", "make", 0).finish(ok = false, "exit 1")
      assert(failed.text == "✗ Bash(make) · exit 1")

      // A tick is a mutation of the glass: it changes the glyph and bumps the
      // revision, so the wrap cache re-wraps exactly this block.
      val t1 = Block.tool("Read", "f", 0)
      val t2 = t1.tick
      assert(t1.text != t2.text && t2.rev == t1.rev + 1)
    }

    test("expansion is rows of the same block: sibling positions and keys do not move") {
      val tool = Block
        .tool("Read", "src/Main.scala", 0)
        .finish(ok = true, "42 lines", Vector("line one", "line two"))
      val doc = Doc.of("before").append(tool).append(Block.Text("after"))
      assert(doc.length == 3)

      val open = tool.withExpanded(true)
      assert(open.text == "✓ Read(src/Main.scala) · 42 lines\n  line one\n  line two")

      // The property the whole design rests on: the block count is constant, so a
      // DocPos into any *other* block is bit-identical before and after, and the
      // siblings' revisions -- the wrap cache keys -- are untouched.
      assert(doc.textAt(0) == "before" && doc.textAt(2) == "after")
      val docOpen = Doc.of("before").append(open).append(Block.Text("after"))
      assert(docOpen.length == doc.length)
      assert(docOpen.textAt(2) == doc.textAt(2))
      assert(docOpen.entry(0).map(_.rev) == doc.entry(0).map(_.rev))
      assert(docOpen.entry(2).map(_.rev) == doc.entry(2).map(_.rev))
      assert(open.rev == tool.rev + 1)

      // A selection spanning all three blocks copies the same text either way.
      val whole = Selection.between(DocPos(0, 0), DocPos(2, 5))
      assert(doc.textOf(whole) == "before\n✓ Read(src/Main.scala) · 42 lines\nafter")
      assert(docOpen.textOf(whole) == "before\n" + open.text + "\nafter")

      // And the viewport: the expanded block simply paints more rows under entry 1,
      // while the rows of entry 2 keep their provenance.
      val (vp1, _) = pane(doc, 60).viewport(Size(10, 60))
      val (vp2, _) = pane(docOpen, 60).viewport(Size(10, 60))
      assert(vp1.rows.map(_.entry) == Vector(0, 1, 2))
      assert(vp2.rows.map(_.entry) == Vector(0, 1, 1, 1, 2))
      assert(vp2.rows(3).text == "  line two")
      assert(vp2.rows.last == vp1.rows.last)
    }

    test("a truncated diff never splits a glyph, and copies whole") {
      // "+" plus four wide glyphs is 9 display columns; at 7 the line truncates to
      // the last glyph boundary, never mid-glyph.
      val doc = Doc.empty.append(Block.Diff(Vector("+世世世世"), Overflow.Truncate))
      val (vp, _) = pane(doc, 7).viewport(Size(4, 7))
      assert(vp.rows.map(_.text) == Vector("+世世世"))
      // Display truncation is exactly that: the logical text, and so the copy, is whole.
      val whole = Selection.between(DocPos(0, 0), DocPos(0, 100))
      assert(doc.textOf(whole) == "+世世世世")
    }

    test("diff overflow is a choice: wrap lays rows out, truncate keeps one per line") {
      val long = Vector("aaa bbb ccc ddd")
      val trunc = Doc.empty.append(Block.Diff(long, Overflow.Truncate))
      val wrapped = Doc.empty.append(Block.Diff(long, Overflow.Wrap))
      val (vpT, _) = pane(trunc, 7).viewport(Size(4, 7))
      val (vpW, _) = pane(wrapped, 7).viewport(Size(4, 7))
      assert(vpT.rows.map(_.text) == Vector("aaa bbb"))
      assert(vpW.rows.map(_.text) == Vector("aaa bbb", "ccc ddd"))
      assert(vpW.rows(1).start == 8)
    }

    test("a separator paints a rule across the width and copies as an empty line") {
      val doc = Doc
        .of("answer")
        .append(Block.Separator())
        .append(Block.Text("next prompt"))
      val (vp, p) = pane(doc, 12).viewport(Size(4, 12))
      assert(vp.rows.map(_.text) == Vector("answer", "", "next prompt"))
      assert(vp.rows(1).rule)

      val painted = vp.render(None)
      val rule = (0 until 12).map(c => painted.at(1, c).ch).mkString
      assert(rule == "────────────")

      // A click on the rule lands at the start of an empty logical row, and a
      // selection spanning it copies the separator as an empty line -- it is a rule,
      // not content.
      assert(vp.docPosAt(Pos(1, 4)) == Some(DocPos(1, 0)))
      val all = Selection.between(DocPos(0, 0), DocPos(2, 11))
      assert(doc.textOf(all) == "answer\n\nnext prompt")
    }

    test("a mixed transcript lays out as rows with provenance") {
      val doc = Doc
        .of("hello world")
        .append(Block.tool("Read", "f", 0))
        .append(Block.Diff(Vector("+import x", "-import y")))
      val (vp, _) = pane(doc, 40).viewport(Size(6, 40))
      assert(
        vp.rows.map(r => (r.entry, r.start, r.text)) == Vector(
          (0, 0, "hello world"),
          (1, 0, "⠋ Read(f)"),
          (2, 0, "+import x"),
          (2, 10, "-import y")
        )
      )
      // docPosAt still inverts what was painted, diff rows included.
      assert(vp.docPosAt(Pos(3, 1)) == Some(DocPos(2, 11)))
    }
  }
}
