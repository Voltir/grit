package grit.tui.model.text

import grit.tui.model.surface.{Color, Style}
import utest.*

/** Spans index the logical, unwrapped text, and [[Span.rebase]] is the one operation
  * that moves them into a wrapped row's coordinates. If it is wrong, a style lands on
  * the wrong characters and nothing else in the system notices.
  */
object SpanTests extends TestSuite {

  private val red = Style.fg(Color.hex("#ff0000"))
  private val blue = Style.fg(Color.hex("#0000ff"))

  val tests = Tests {

    test("a span inside one row is shifted, not resized") {
      val spans = Vector(Span(12, 17, red))
      assert(Span.rebase(spans, 10, 10) == Vector(Span(2, 7, red)))
    }

    test("a span is clipped at both ends of the row it lands in") {
      // The case a wrapped paragraph makes constantly: one span over a whole block, cut
      // into as many pieces as the block has rows.
      val whole = Vector(Span(0, 30, red))
      assert(Span.rebase(whole, 0, 10) == Vector(Span(0, 10, red)))
      assert(Span.rebase(whole, 10, 10) == Vector(Span(0, 10, red)))
      assert(Span.rebase(whole, 20, 10) == Vector(Span(0, 10, red)))
      // ...and the row past its end gets nothing, not a zero-width span.
      assert(Span.rebase(whole, 30, 10) == Vector.empty)
    }

    test("a span entirely before or after the row is dropped") {
      assert(Span.rebase(Vector(Span(0, 5, red)), 10, 10) == Vector.empty)
      assert(Span.rebase(Vector(Span(40, 50, red)), 10, 10) == Vector.empty)
      // Touching the boundary is not overlapping it.
      assert(Span.rebase(Vector(Span(0, 10, red)), 10, 10) == Vector.empty)
      // ...and nothing to rebase is nothing rebased, which is the same answer.
      assert(Span.rebase(Vector.empty, 0, 10) == Vector.empty)
    }

    test("order is preserved, so a later span still layers over an earlier one") {
      // Painting applies spans in order and each layers over what is under it, so a
      // ground written first and an accent written second must arrive in that order.
      val spans = Vector(Span(0, 20, red), Span(3, 6, blue))
      assert(Span.rebase(spans, 0, 20) == Vector(Span(0, 20, red), Span(3, 6, blue)))
    }

    test("StyledText concatenation shifts the right-hand spans") {
      val a = StyledText.styled("abc", red)
      val b = StyledText.styled("de", blue)
      val j = a ++ b
      assert(j.text == "abcde")
      assert(j.spans == Vector(Span(0, 3, red), Span(3, 5, blue)))
    }

    test("under puts a style beneath what is already there") {
      val t = StyledText.styled("abc", red).under(blue)
      assert(t.spans == Vector(Span(0, 3, blue), Span(0, 3, red)))
      // ...and a plain ground is not a span at all.
      assert(StyledText.styled("abc", red).under(Style.plain).spans.length == 1)
    }

    test("styling with plain produces no span") {
      assert(StyledText.styled("abc", Style.plain).spans.isEmpty)
    }

    test("of concatenates pieces in order") {
      val t = StyledText.of(
        StyledText.styled("a", red),
        StyledText("bb"),
        StyledText.styled("c", blue)
      )
      assert(t.text == "abbc")
      assert(t.spans == Vector(Span(0, 1, red), Span(3, 4, blue)))
    }
  }
}
