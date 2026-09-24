package grit.tui.components.view

import grit.tui.components.editor.Editor
import grit.tui.components.layout.{Border, Box}
import grit.tui.components.overlay.Popup
import grit.tui.components.widget.{Scrollbar, Spinner, StatusBar}
import grit.tui.model.surface.{Frame, Size, Surface}
import grit.tui.model.text.Width
import grit.tui.wire.paint.Painter

import utest.*

object ViewTests extends TestSuite {

  /** A leaf view that fills its box, so composition can be checked by reading glyphs. */
  private final case class Fill(ch: Char, want: Size = Size(1, 1)) extends View {
    def measure(avail: Size): Size = want
    def render(size: Size): Surface = Surface.filled(size, grit.tui.model.surface.Cell(ch))
  }

  /** Every concrete view in the library, so the size law is asserted over all of them
    * rather than over whichever one was written last.
    */
  private val everyView: Vector[View] = Vector(
    Spinner(3),
    StatusBar(Vector("left", "here"), Vector("9 rows", "cached")),
    Scrollbar(100, 10, 45),
    Popup.of("/commit", "/compact", "/help"),
    Editor("some draft text that is long enough to wrap somewhere", caret = 7),
    Box(Fill('x', Size(4, 9)), title = "titled"),
    Fill('y', Size(3, 3))
  )

  /** Sizes chosen to include the degenerate ones a real terminal produces on the way
    * to a usable size: zero, one row, one column, narrower than any chrome.
    */
  private val sizes: Vector[Size] = Vector(
    Size(0, 0),
    Size(0, 10),
    Size(10, 0),
    Size(1, 1),
    Size(1, 40),
    Size(40, 1),
    Size(2, 2),
    Size(3, 8),
    Size(5, 20),
    Size(24, 80)
  )

  val tests = Tests {

    test("a view paints exactly the box it was given, and never outside it") {
      // The law layoutz cannot state: its alignment wrappers refuse to shrink
      // (`if (lineLength >= targetWidth) line`) and nothing clips. Here a view that
      // ignored its argument, or grew to fit its content, fails on the first size.
      var checked = 0
      everyView.foreach { v =>
        sizes.foreach { size =>
          val s = v.render(size)
          assert(s.size == size)
          assert(s.cells.length == math.max(0, size.rows * size.cols))
          checked += 1
        }
      }
      assert(checked == everyView.length * sizes.length)
    }

    test("measure is declared, not derived from a render") {
      // layoutz's `width` re-renders to measure, so every child paints twice a frame.
      // A measure that is cheap is a measure a parent can afford to ask for.
      assert(Spinner(0).measure(Size(24, 80)) == Size(1, 1))
      assert(Scrollbar(100, 10, 0).measure(Size(24, 80)) == Size(24, 1))
      assert(StatusBar(Vector("a"), Vector("b")).measure(Size(24, 80)) == Size(1, 80))
      // A box wants its child plus one cell of frame on each side.
      assert(Box(Fill('x', Size(4, 9))).measure(Size(24, 80)) == Size(6, 11))
      // and never more than it was offered.
      assert(Box(Fill('x', Size(99, 99))).measure(Size(10, 12)) == Size(10, 12))
    }

    test("an editor reports the height its draft wants, and still paints its box") {
      // The two halves of the seam, and the whole of what makes a growing prompt
      // legal: `measure` is a *report*, and the region above decides what to do with
      // it. Both are asserted here because either one alone would be a bug -- a view
      // that sized itself fails the law above, and a view that reported `avail` is
      // what `Region.Fit` has nothing to ask.
      val empty = Editor("", 0)
      assert(empty.measure(Size(24, 20)) == Size(3, 20)) // two borders and a blank row
      val two = Editor("x" * 20, 0) // 18 columns inside the border: two wrapped rows
      assert(two.measure(Size(24, 20)) == Size(4, 20))
      assert(Editor("x" * 180, 0).measure(Size(24, 20)) == Size(12, 20))
      // never more than it was offered, however long the draft.
      assert(Editor("x" * 9000, 0).measure(Size(24, 20)) == Size(24, 20))
      // and it paints exactly the box it is handed, at a height it did not ask for.
      assert(Editor("x" * 180, 0).render(Size(3, 20)).size == Size(3, 20))
    }

    test("a box frames its child and keeps the child inside the frame") {
      val b = Box(Fill('x'), title = "help")
      val s = b.render(Size(5, 12))
      val lines = s.lines
      assert(lines.head.startsWith("╭─ help ") && lines.head.endsWith("╮"))
      assert(lines.last == "╰" + "─" * 10 + "╯")
      // Every content row is a rail, the child, a rail -- the child never overwrites
      // the frame, which is what `inset` is for.
      (1 until 4).foreach { r =>
        assert(lines(r) == "│" + "x" * 10 + "│")
      }
    }

    test("a box title is cut in display columns, never mid-glyph") {
      val b = Box(Fill('x'), title = "a中中中中中中b")
      val s = b.render(Size(3, 14))
      // A cell holds one char and a wide glyph spans two columns, so the row is 14
      // cells but fewer chars: measure the drawn part the way the terminal will.
      val top = s.lines.head.reverse.dropWhile(_ == ' ').reverse
      assert(Width.of(top) == 14) // the wide glyphs did not push the frame wider
      assert(top.startsWith("╭─ ") && top.endsWith("╮"))
      assert(!top.contains("b")) // the tail was cut, and cut between glyphs
    }

    test("the borders are the four corner sets, and nothing else changes") {
      val round = Box(Fill('x'), border = Border.Round).render(Size(3, 4)).lines
      val plain = Box(Fill('x'), border = Border.Plain).render(Size(3, 4)).lines
      assert(round.head == "╭──╮" && round.last == "╰──╯")
      assert(plain.head == "┌──┐" && plain.last == "└──┘")
      assert(round(1) == "│xx│" && plain(1) == "│xx│")
    }

    test("no view writes a byte for a frame it already painted") {
      // What five per-component copies of this used to say one component at a time.
      // Stated over every view at every size, it is a claim they never made: the
      // painter's diff is driven by cell equality, so a view whose render depended on
      // anything but its own fields would write bytes for a frame nobody changed --
      // at some size, if not at the obvious one.
      var checked = 0
      everyView.foreach { v =>
        sizes.foreach { size =>
          val once = Frame(v.render(size))
          val again = Frame(v.render(size))
          assert(Painter.paint(again, Some(once)) == "")
          checked += 1
        }
      }
      assert(checked == everyView.length * sizes.length)
    }
  }
}
