package grit.tui.model.text

import utest.*

object WidthTests extends TestSuite {

  // The shapes that break a naive width function: wide, combining, astral-narrow, emoji.
  private val mixed = "a世b\u0301\uD835\uDC00 \uD83D\uDE00"

  val tests = Tests {

    test("an astral narrow character is one cell, not two surrogate halves") {
      // The bug the slice exists for: counting UTF-16 chars measures U+1D400 as 2.
      assert(Width.of("\uD835\uDC00") == 1)
    }

    test("a column inside a wide glyph resolves to the glyph's start, never inside it") {
      // The selection-drift contract: hit-testing the second cell of a wide glyph
      // must address that glyph, not its low surrogate. "a世b": a=col 0, 世=cols 1-2, b=col 3.
      assert(Width.offsetAtColumn("a世b", 2) == 1)
      assert(Width.offsetAtColumn("a世b", 3) == 2)
    }

    test("columns and offsets round-trip on every visible code point start") {
      // The property wrapping, hit-testing and selection all share: mapping a visible
      // code point's offset to its column and back lands on the same code point; a
      // zero-width code point maps forward to the next visible one; columns never
      // move backwards.
      var i = 0
      var prevCol = -1
      while (i < mixed.length) {
        val cp = mixed.codePointAt(i)
        val col = Width.columnAtOffset(mixed, i)
        assert(col >= prevCol)
        val back = Width.offsetAtColumn(mixed, col)
        if (Width.ofCodePoint(cp) == 0) assert(back > i)
        else assert(back == i)
        prevCol = col
        i += Character.charCount(cp)
      }
    }

    test("zero-width and astral code points never open a new column") {
      // e + combining acute is one cell; so is U+1D400 -- column 1 starts at 'x'.
      val s = "e\u0301\uD835\uDC00x"
      assert(Width.columnAtOffset(s, 2) == 1)
      assert(Width.columnAtOffset(s, 3) == 2)
      assert(Width.columnAtOffset(s, 4) == 2)
    }
  }
}
