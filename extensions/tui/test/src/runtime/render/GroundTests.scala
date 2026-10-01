package grit.tui.runtime.render

import grit.tui.components.tree.Node
import grit.tui.components.tree.Node.*
import grit.tui.components.widget.StatusBar
import grit.tui.model.surface.{Color, Size, Style}

import utest.*

/** A ground paints the background a screen leaves to the terminal, and nothing else. */
object GroundTests extends TestSuite {

  private val Night = Color.hex("#0d1319")
  private val Slab = Color.hex("#1e2a37")

  val tests = Tests {

    test("a grounded screen paints its blank cells, and its bars keep their own colours") {
      val bar = Style.bg(Slab)
      val root: Node[Nothing] = column(
        fixed(1) -> paint(StatusBar(Vector("grit"), Vector(), bar)),
        flex() -> paint(StatusBar(Vector(), Vector(), Style.plain))
      ).grounded(Style.bg(Night))
      val (frame, _, _) = grit.tui.runtime.render.Paint.frame(root, Size(3, 10), Memo.empty)
      frame.surface.at(0, 5).style.bg ==> Some(Slab)
      frame.surface.at(2, 5).style.bg ==> Some(Night)
      frame.surface.at(2, 9).style.bg ==> Some(Night)
    }
  }
}
