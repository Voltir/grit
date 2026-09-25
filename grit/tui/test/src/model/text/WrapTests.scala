package grit.tui.model.text

import utest.*

object WrapTests extends TestSuite {

  /** The provenance oracle: every row's text is exactly the slice of the logical text
    * its startOffset points at. This is what lets a selection be copied as a slice of
    * the author's text rather than of the wrapped viewport.
    */
  private def assertSlices(text: String)(rows: Vector[Row]): Unit = rows.foreach { r =>
    assert(r.startOffset >= 0)
    assert(r.startOffset + r.text.length <= text.length)
    assert(text.slice(r.startOffset, r.startOffset + r.text.length) == r.text)
  }

  /** Every row must fit the width in display cells -- wide glyphs included. */
  private def assertFits(rows: Vector[Row], width: Int): Unit = rows.foreach { r =>
    assert(Width.of(r.text) <= math.max(1, width))
  }

  val tests = Tests {

    test("a soft break lands on the last space that fits and consumes it") {
      val rows = Wrap.wrap("one two three", 7)
      assertSlices("one two three")(rows)
      assertFits(rows, 7)
      assert(rows.map(_.text) == Vector("one two", "three"))
    }

    test("a hard break measures display columns, not characters") {
      // 世 is two cells; three of them are six cells, so width 4 fits exactly two.
      val rows = Wrap.wrap("世世世", 4)
      assertSlices("世世世")(rows)
      assertFits(rows, 4)
      assert(rows.map(_.text) == Vector("世世", "世"))
    }

    test("a soft break and a hard break can occur in the same logical line") {
      val text = "aa bb ccccdddd"
      val rows = Wrap.wrap(text, 6)
      assertSlices(text)(rows)
      assertFits(rows, 6)
      assert(rows.map(_.text) == Vector("aa bb", "ccccdd", "dd"))
    }

    test("truncation keeps one row per line, cut at a glyph boundary, with its provenance") {
      val text = "abc def ghi\n\n世世世世"
      val rows = Wrap.truncate(text, 5)
      assertSlices(text)(rows)
      assertFits(rows, 5)
      assert(rows == Vector(Row(0, "abc d"), Row(12, ""), Row(13, "世世")))
    }

    test("newlines are honoured, offsets continue across them, and an empty line is a row") {
      val rows = Wrap.wrap("ab\n\ncd", 10)
      assertSlices("ab\n\ncd")(rows)
      assert(rows == Vector(Row(0, "ab"), Row(3, ""), Row(4, "cd")))
    }

    test("a width below one is clamped to one") {
      // Load-bearing: width 0 would make every hard break land on the same offset.
      val rows = Wrap.wrap("ab", 0)
      assertSlices("ab")(rows)
      assert(rows.map(_.text) == Vector("a", "b"))
    }

    test("every row of a mixed document fits and slices back to its source") {
      // Soft breaks, hard breaks, wide glyphs, emoji and newlines in one text.
      val text = "seed message 世界\nindented  \uD83D\uDE00 entry\n0123456789abcdef"
      val rows = Wrap.wrap(text, 8)
      assertSlices(text)(rows)
      assertFits(rows, 8)
    }

    test("the first row gets the first width, every later row the rest") {
      val text = "one two three four five six"
      val rows = Wrap.wrap(text, 8, 5)
      rows.map(_.text) ==> Vector("one two", "three", "four", "five", "six")
      assertSlices(text)(rows)
      // A later logical line is a later row too: it wraps at the rest.
      Wrap.wrap("ab cd\nef gh", 5, 2).map(_.text) ==> Vector("ab cd", "ef", "gh")
      Wrap.truncate("abcdef\nabcdef", 4, 2).map(_.text) ==> Vector("abcd", "ab")
    }
  }
}
