package grit.tui.components.widget

import grit.tui.wire.paint.Painter
import grit.tui.model.surface.{Frame, Size}
import utest.*

object SpinnerTests extends TestSuite {

  private val glyph = (tick: Long) => Spinner(tick).render(Size(1, 1)).at(0, 0).ch

  val tests = Tests {

    test("the glyph cycles through the frames in tick order, one cell") {
      val s = Spinner(2).render(Size(1, 1))
      assert(s.size == Size(1, 1))
      assert(glyph(0) == Spinner.frames(0))
      assert(glyph(1) == Spinner.frames(1))
      assert(glyph(2) == Spinner.frames(2))
      assert(glyph(0) != glyph(1)) // it is actually animating
    }

    test("a tick that changes nothing writes zero bytes") {
      // Ten ticks later the frame is back where it started; the painter must see
      // two identical surfaces, not ten frames of churn.
      val f1 = Frame(Spinner(0).render(Size(1, 1)))
      val f2 = Frame(Spinner(Spinner.frames.length).render(Size(1, 1)))
      assert(f1.surface == f2.surface)
      assert(Painter.paint(f2, Some(f1)) == "")
    }
  }
}
