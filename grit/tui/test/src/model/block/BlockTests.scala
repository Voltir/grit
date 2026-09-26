package grit.tui.model.block

import utest.*

/** What a block's text is, before any layout: a tool call's summary line and a
  * separator's rule. How blocks lay out as rows is `grit.tui.runtime.render.BlockLayoutTests`'.
  */
object BlockTests extends TestSuite {

  val tests = Tests {

    test("a tool call is one summary line, and the glyph tells the state") {
      val running = Block.tool("Read", "src/Main.scala", tick = 3)
      assert(running.text == "⠸ Read(src/Main.scala)")

      val done = running.finish(ok = true, "3 files, 42 lines")
      assert(done.text == "✓ Read(src/Main.scala) · 3 files, 42 lines")

      val failed = Block.tool("Bash", "make", 0).finish(ok = false, "exit 1")
      assert(failed.text == "✗ Bash(make) · exit 1")

      // A tick is a mutation of the glass: it moves the glyph to the next frame, so the
      // block is no longer equal to itself and the wrap memo re-wraps exactly this block.
      assert(Block.tool("Read", "f", 0).tick.text == "⠙ Read(f)")
    }

    test("a separator draws its own line, with a mark centred on it when there is room") {
      Block.Separator(line = '━', mark = "᛭").drawn(11) ==> "━━━━ ᛭ ━━━━"
      Block.Separator(line = '━', mark = "᛭").drawn(6) ==> "━━━━━━"
      Block.Separator().drawn(3) ==> "───"
    }
  }
}
