package grit.tui.components.overlay

import grit.tui.model.input.{Button, Input, Key, Mods, MouseEvent, MouseKind}
import grit.tui.model.surface.{Color, Pos, Rect, Size, Style, Surface}
import grit.tui.model.text.Width
import utest.*

object ModalTests extends TestSuite {

  private val modal = Modal("help", rows = 9, cols = 46)
  private val screen = Size(30, 100)

  /** A screen with something legible on every cell, so "dimmed, not cleared" is a
    * question the painted grid can answer.
    */
  private def app(size: Size): Surface =
    (0 until size.rows).foldLeft(Surface.blank(size)) { (s, r) => s.write(r, 0, "x" * size.cols) }

  private def press(row: Int, col: Int): Input =
    Input.Mouse(MouseEvent(MouseKind.Press, Button.Left, Pos(row, col), Mods.none))

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

    test("no shadow is cast") {
      // The shade glyph is gone, and with it the `fill` that destroyed the app's glyphs
      // to fake depth. The cells a shadow used to occupy are the app's, dimmed like the
      // rest of the backdrop.
      val painted = modal.render(app(screen), body)
      val o = modal.outer(body)
      Vector(
        Pos(o.top + 1, o.right),
        Pos(o.bottom, o.left + 1),
        Pos(o.bottom, o.right)
      ).foreach { p =>
        assert(painted.at(p.row, p.col).ch == 'x')
        assert(painted.at(p.row, p.col).style.dim)
      }
      assert(!painted.lines.exists(_.contains("░")))
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
      // ...and the panel is not the backdrop's ground.
      assert(!painted.at(0, 0).style.bg.contains(Panel))
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

    test("a title too long for the frame is cut in display columns") {
      val size = Size(12, 24)
      val wide = Modal("a中中中中中中b", rows = 3, cols = 10)
      val b = wide.place(size).get
      val painted = wide.render(app(size), b)
      val o = wide.outer(b)
      // A cell holds one char and a wide glyph spans two columns, so the border is
      // measured the way the terminal will measure it: in display columns.
      val drawn = painted.lines(o.top).slice(o.left, o.right).reverse.dropWhile(_ == ' ').reverse
      assert(Width.of(drawn) == o.cols) // the wide glyphs did not push the frame wider
      assert(drawn.startsWith("╭─ ") && drawn.endsWith("╮"))
      assert(!drawn.contains("b")) // the tail was cut, and cut between glyphs
    }

    test("nothing reaches the app beneath: capture is the type, not a guard") {
      // The oracle for "input capture as a routing rule": every interaction has a
      // route, and none of them is a way through. Only the terminal's own facts are
      // ambient, and this asserts which by enumerating the alternative.
      val interactions = Vector(
        Input.Keyboard(Key.Printable('a')),
        Input.Keyboard(Key.Ctrl('c')),
        Input.Keyboard(Key.Enter),
        Input.Keyboard(Key.Tab),
        Input.Keyboard(Key.Backspace),
        Input.Keyboard(Key.Left()),
        Input.Keyboard(Key.Unknown("[99~")),
        Input.Paste("pasted"),
        press(body.top, body.left),
        press(0, 0),
        press(screen.rows - 1, screen.cols - 2),
        Input.Mouse(MouseEvent(MouseKind.Move, Button.None, Pos(0, 0), Mods.none)),
        Input.Mouse(MouseEvent(MouseKind.Wheel, Button.WheelUp, Pos(0, 0), Mods.none))
      )
      val leaked = interactions.filter(i =>
        modal.route(i, body) match {
          case Modal.Route.Ambient(_) => true
          case _ => false
        }
      )
      assert(leaked.isEmpty)
      val ambient = Vector(Input.Resize(Size(1, 1)), Input.Focus(true))
      val captured = ambient.filter(i =>
        modal.route(i, body) match {
          case Modal.Route.Ambient(_) => false
          case _ => true
        }
      )
      assert(captured.isEmpty)
    }

    test("a click on the frozen app is swallowed, a click in the body is not") {
      val inside = Pos(body.top, body.left)
      assert(modal.route(press(inside.row, inside.col), body) == Modal.Route.Pointer(inside))
      assert(modal.route(press(0, 0), body) == Modal.Route.Swallowed)
      assert(modal.route(press(body.top, body.left - 1), body) == Modal.Route.Swallowed)
    }

    test("a drag already in flight is followed out of the modal") {
      // Rule 6 says the pane the drag began in clamps it; swallowing the motion would
      // instead strand it -- the pointer leaves the frame and the selection freezes, and
      // the release never arrives, so nothing is ever copied.
      val outside = Pos(0, 0)
      val drag = Input.Mouse(MouseEvent(MouseKind.Drag, Button.Left, outside, Mods.none))
      val release = Input.Mouse(MouseEvent(MouseKind.Release, Button.Left, outside, Mods.none))
      assert(modal.route(drag, body) == Modal.Route.Pointer(outside))
      assert(modal.route(release, body) == Modal.Route.Pointer(outside))
    }

    test("the scrolling keys address the body, and a page is the body's height") {
      assert(modal.route(Input.Keyboard(Key.Up()), body) == Modal.Route.Scroll(-1))
      assert(modal.route(Input.Keyboard(Key.Down()), body) == Modal.Route.Scroll(1))
      assert(modal.route(Input.Keyboard(Key.PageUp()), body) == Modal.Route.Scroll(-body.rows))
      assert(modal.route(Input.Keyboard(Key.PageDown()), body) == Modal.Route.Scroll(body.rows))
      assert(modal.route(Input.Keyboard(Key.Home()), body) == Modal.Route.ToTop)
      assert(modal.route(Input.Keyboard(Key.End()), body) == Modal.Route.ToBottom)
      assert(modal.route(Input.Keyboard(Key.Escape), body) == Modal.Route.Close)
      val up = Input.Mouse(MouseEvent(MouseKind.Wheel, Button.WheelUp, Pos(0, 0), Mods.none))
      val down = Input.Mouse(MouseEvent(MouseKind.Wheel, Button.WheelDown, Pos(0, 0), Mods.none))
      assert(modal.route(up, body) == Modal.Route.Scroll(-3))
      assert(modal.route(down, body) == Modal.Route.Scroll(3))
    }
  }
}
