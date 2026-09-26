package grit.tui.components.overlay

import grit.tui.model.surface.{Color, Pos, Rect, Size, Style, Surface}

import utest.*

object ModalTests extends TestSuite {

  private val modal = Modal("help", rows = 9, cols = 46)
  private val screen = Size(30, 100)

  /** A screen with something legible on every cell, so "dimmed, not cleared" is a
    * question the painted grid can answer.
    */
  private def app(size: Size): Surface =
    (0 until size.rows).foldLeft(Surface.blank(size)) { (s, r) => s.write(r, 0, "x" * size.cols) }

  private val body = modal.place(screen).get

  val tests = Tests {

    test("the body is centred and the chrome fits around it") {
      // Every size the modal will take: the frame stays on the screen and the body sits
      // in the middle of what is left, within the rounding.
      var checked = 0
      var rows = 9
      while (rows <= 40) {
        var cols = 24
        while (cols <= 120) {
          val size = Size(rows, cols)
          modal.place(size) match {
            case None => ()
            case Some(b) =>
              val o = modal.outer(b)
              assert(o.top >= 0 && o.left >= 0)
              assert(o.bottom <= size.rows && o.right <= size.cols)
              assert(math.abs(o.top - (size.rows - o.bottom)) <= 1)
              assert(math.abs(o.left - (size.cols - o.right)) <= 1)
              checked += 1
          }
          cols += 7
        }
        rows += 3
      }
      assert(checked > 40)
    }

    test("a fitting modal is as tall as its content, up to its rows; a fixed one ignores it") {
      val fitting = modal.copy(fit = true)
      fitting.place(screen, 4).map(_.rows) ==> Some(4)
      fitting.place(screen, 40).map(_.rows) ==> Some(9)
      fitting.place(screen, 0).map(_.rows) ==> Some(1)
      fitting.place(Size(12, 100), 8).map(_.rows) ==> Some(6) // and still the screen's
      modal.place(screen, 4).map(_.rows) ==> Some(9)
    }

    test("a screen too small for a usable dialog gets none") {
      assert(modal.place(Size(8, 100)).isEmpty)
      assert(modal.place(Size(30, 23)).isEmpty)
      assert(modal.place(Size(0, 0)).isEmpty)
      assert(modal.place(Size(9, 24)).isDefined)
    }

    test("the app beneath is dimmed, not cleared") {
      // "It is frozen, not gone": the glyphs outside the frame survive and only their
      // style changes. Clearing the backdrop would pass a test that only asked whether
      // the modal was on top.
      val painted = modal.render(app(screen), body)
      assert(painted.at(0, 0).ch == 'x')
      assert(painted.at(0, 0).style.dim)
      val o = modal.outer(body)
      assert(!painted.at(o.top, o.left).style.dim) // the frame itself is not dim
      assert(!painted.at(body.top, body.left).style.dim)
      // Every cell outside the frame keeps its glyph. Nothing is excused: with the
      // shadow gone there is no rect the modal is allowed to overwrite.
      var kept = 0
      (0 until screen.rows).foreach { r =>
        (0 until screen.cols).foreach { c =>
          if (!o.contains(Pos(r, c))) {
            assert(painted.at(r, c).ch == 'x' && painted.at(r, c).style.dim)
            kept += 1
          }
        }
      }
      assert(kept > 2000)
    }

    test("a dressed modal lights its panel and pushes the app back") {
      // What replaces the shadow: the frame reads as raised because it is lit and the
      // app behind it recedes, not because a glyph was stamped down and to the right.
      val Panel = Color.hex("#24283b")
      val Ink = Color.hex("#bb9af7")
      val Behind = Color.hex("#16161e")
      val dressed = modal.copy(
        panel = Style.bg(Panel),
        chrome = Style.fg(Ink),
        behind = Style.bg(Behind) + Style.Dim
      )
      val painted = dressed.render(app(screen), body)
      val o = modal.outer(body)
      // The panel is the ground of the frame and of the body left for the app.
      assert(painted.at(body.top, body.left).style.bg.contains(Panel))
      assert(painted.at(o.top, o.left).style.bg.contains(Panel))
      assert(painted.at(o.top, o.left).style.fg.contains(Ink))
      // The backdrop keeps its glyphs and gains both the ground and the dim.
      assert(painted.at(0, 0).ch == 'x')
      assert(painted.at(0, 0).style.bg.contains(Behind))
      assert(painted.at(0, 0).style.dim)
    }

    test("the frame is drawn around the body, and the body is left for the app") {
      val painted = modal.render(app(screen), body)
      val o = modal.outer(body)
      val top = painted.lines(o.top).slice(o.left, o.right)
      assert(top.startsWith("╭─ help ─"))
      assert(top.endsWith("╮"))
      val bottom = painted.lines(o.bottom - 1).slice(o.left, o.right)
      assert(bottom == "╰" + "─" * (o.cols - 2) + "╯")
      (body.top until body.bottom).foreach { r =>
        assert(painted.at(r, o.left).ch == '│' && painted.at(r, o.right - 1).ch == '│')
        assert(painted.lines(r).slice(body.left, body.right).forall(_ == ' '))
      }
    }
  }
}
