package grit.tui.components.overlay

import grit.tui.model.input.{Button, Input, Key, Mods, MouseEvent, MouseKind}
import grit.tui.model.surface.{Pos, Rect, Size}
import grit.tui.model.text.Width

import utest.*

object PopupTests extends TestSuite {

  private val commands =
    Popup.of("/commit", "/compact", "/context", "/diff", "/help", "/history", "/quit")

  /** The prompt box the completion list hangs off: three rows at the bottom. */
  private val prompt = Rect(21, 0, 3, 60)
  private val screen = Size(24, 60)

  private def press(row: Int, col: Int): Input =
    Input.Mouse(MouseEvent(MouseKind.Press, Button.Left, Pos(row, col), Mods.none))

  val tests = Tests {

    test("the query filters, and the selection is clamped into what is left") {
      val p = commands.withQuery("/co")
      assert(p.matches == Vector("/commit", "/compact", "/context"))
      // Selected past the end of the narrower list: clamped, not left dangling.
      val far = commands.copy(selected = 6).withQuery("/co")
      assert(far.selected == 2)
      assert(far.choice.contains("/context"))
      assert(commands.withQuery("/CO").matches.length == 3) // case-insensitive
      assert(commands.withQuery("/zz").isEmpty)
      assert(commands.withQuery("/zz").choice.isEmpty)
    }

    test("the list is a ring") {
      assert(commands.move(1).choice.contains("/compact"))
      assert(commands.move(-1).choice.contains("/quit")) // wrapped off the top
      assert(commands.move(7).choice.contains("/commit"))
      assert(commands.withQuery("/zz").move(1).choice.isEmpty) // nothing to move through
    }

    test("the window is bounded and the selection is always inside it") {
      // The window is derived from the selection rather than stored, so this holds at
      // every selection by construction -- there is no offset to fall out of step.
      val p = commands.copy(maxRows = 3)
      var i = 0
      while (i < p.matches.length) {
        val at = p.copy(selected = i)
        assert(at.rows == 3)
        assert(at.visible.length == 3)
        assert(at.selectedRow >= 0 && at.selectedRow < 3)
        assert(at.visible(at.selectedRow) == at.matches(i))
        i += 1
      }
      // Fewer matches than the bound: the box shrinks to them.
      assert(commands.copy(maxRows = 3).withQuery("/q").rows == 1)
    }

    test("the box hangs above the prompt, and flips below when there is no room") {
      val p = commands.copy(maxRows = 4)
      val box = p.place(prompt, screen).get
      assert(box.bottom == prompt.top) // sitting on the prompt
      assert(box.left == prompt.left)
      assert(box.rows == p.rows + 2)
      assert(box.cols == Width.of("/history") + 2)
      // A prompt at the top of the screen: the list goes below rather than off-screen.
      val high = Rect(0, 0, 3, 60)
      val flipped = p.place(high, screen).get
      assert(flipped.top == high.bottom)
      // Clamped into the screen in both axes, whatever it is anchored to.
      val edge = Rect(1, 58, 3, 2)
      val clamped = p.place(edge, screen).get
      assert(clamped.right <= screen.cols && clamped.bottom <= screen.rows)
      assert(clamped.left >= 0 && clamped.top >= 0)
      assert(commands.withQuery("/zz").place(prompt, screen).isEmpty)
    }

    test("the list paints one match per row, the selection in reverse") {
      val p = commands.copy(maxRows = 3, selected = 1)
      val box = p.place(prompt, screen).get
      val s = p.render(Size(box.rows, box.cols))
      val lines = s.lines
      assert(lines.length == 5)
      assert(lines.head.startsWith("╭") && lines.head.endsWith("╮"))
      assert(lines.last.startsWith("╰") && lines.last.endsWith("╯"))
      assert(lines(1).slice(1, 8) == "/commit")
      assert(lines(2).slice(1, 9) == "/compact")
      // The mask is the selection, and it covers the whole row, not just the glyphs --
      // a highlight the width of the word is a different widget.
      val mask = (0 until s.size.rows).map { r =>
        (0 until s.size.cols).count(c => s.at(r, c).style.reverse)
      }
      assert(mask == Seq(0, 0, box.cols - 2, 0, 0))
    }

    test("a match too wide for the box is cut in display columns") {
      val p = Popup(Vector("中中中中中中"), maxCols = 5)
      val s = p.render(Size(3, 7))
      val row = s.lines(1)
      val content = row.slice(1, 6).reverse.dropWhile(_ == ' ').reverse
      assert(content == "中中") // two whole glyphs, not two and a half
      assert(Width.of(content) <= 5)
      assert(row.charAt(0) == '│' && row.charAt(6) == '│')
    }

    test("typing goes to the editor, the list keys do not") {
      val p = commands.copy(maxRows = 4)
      val box = p.place(prompt, screen).get
      val passed = Vector(
        Input.Keyboard(Key.Printable('c')),
        Input.Keyboard(Key.Backspace),
        Input.Keyboard(Key.Left()),
        Input.Paste("/co")
      )
      assert(passed.forall(i => p.route(i, box) == Popup.Route.Pass(i)))
      assert(p.route(Input.Keyboard(Key.Down()), box) == Popup.Route.Stay(p.move(1)))
      assert(p.route(Input.Keyboard(Key.Up()), box) == Popup.Route.Stay(p.move(-1)))
      assert(p.route(Input.Keyboard(Key.Enter), box) == Popup.Route.Chose("/commit"))
      assert(p.route(Input.Keyboard(Key.Tab), box) == Popup.Route.Chose("/commit"))
      assert(p.route(Input.Keyboard(Key.Escape), box) == Popup.Route.Dismissed)
      // Nothing matches: Enter has nothing to accept and closes rather than hanging on.
      val empty = commands.withQuery("/zz")
      assert(empty.route(Input.Keyboard(Key.Enter), box) == Popup.Route.Dismissed)
    }

    test("the mouse picks a row, and a click outside dismisses") {
      val p = commands.copy(maxRows = 4)
      val box = p.place(prompt, screen).get
      assert(p.route(press(box.top + 1, box.left + 2), box) == Popup.Route.Chose("/commit"))
      assert(p.route(press(box.top + 3, box.left + 2), box) == Popup.Route.Chose("/context"))
      // The frame is the popup too: clicking it keeps the list rather than choosing.
      assert(p.route(press(box.top, box.left), box) == Popup.Route.Stay(p))
      assert(p.route(press(prompt.top + 1, 5), box) == Popup.Route.Dismissed)
      val wheel = Input.Mouse(MouseEvent(MouseKind.Wheel, Button.WheelDown, Pos(0, 0), Mods.none))
      assert(p.route(wheel, box) == Popup.Route.Stay(p.move(1)))
    }
  }
}
