package grit.tui.components.editor

import grit.tui.model.input.{Input, Key, Mods}
import grit.tui.model.surface.{Size, Surface}

import utest.*

object EditorTests extends TestSuite {

  /** The width the editor's box renders at; every method takes this, the editor
    * subtracts its own border.
    */
  private val Box = 12

  private def typed(e: Editor, s: String): Editor = {
    var cur = e
    var i = 0
    while (i < s.length) {
      cur = cur.apply(Input.Keyboard(Key.Printable(s.charAt(i))), Box).get
      i += 1
    }
    cur
  }

  private def key(e: Editor, k: Key, box: Int = Box): Option[Editor] =
    e.apply(Input.Keyboard(k), box)

  val tests = Tests {

    test("typing inserts at the caret; Enter, Tab and Ctrl-C stay the app's") {
      val e = typed(Editor("", 0), "abc")
      assert(e.text == "abc" && e.caret == 3)
      val mid = e.left().insert("X")
      assert(mid.text == "abXc" && mid.caret == 3)
      assert(key(e, Key.Enter) == None)
      assert(key(e, Key.Tab) == None)
      assert(key(e, Key.Ctrl('c')) == None)
    }

    test("a bracketed paste is one insertion, newlines and all") {
      // Grit's probe: pasting a 40-line snippet must not be 40 submissions. The
      // decoder already guarantees one Paste event; the editor inserts it whole,
      // normalizing \r\n the way terminals paste it.
      val e = Editor("", 0).apply(Input.Paste("one\r\ntwo\nthree"), Box).get
      assert(e.text == "one\ntwo\nthree")
      assert(e.caret == 13)
    }

    test("backspace and delete join lines; the bounds are no-ops") {
      assert(Editor("ab\ncd", 2).delete().text == "abcd")
      assert(Editor("ab\ncd", 3).backspace().text == "abcd")
      val start = Editor("ab", 0)
      assert(start.backspace() == start)
      val end = Editor("ab", 2)
      assert(end.delete() == end)
    }

    test("Home and End are per logical line; Ctrl-A/E spell the same thing") {
      val e = Editor("ab\ncdef", 5)
      assert(e.end().caret == 7)
      assert(e.home().caret == 3)
      assert(key(e, Key.Ctrl('a')).get.caret == 3)
      assert(key(e, Key.Ctrl('e')).get.caret == 7)
    }

    test("word-wise motion is bindable: Ctrl-Left/Right through the decoder's mods") {
      // "one two  three": one 0-2, two 4-6, three 9-13; word chars are alnum and '_'.
      val ctrl = Mods(false, false, true)
      val e = Editor("one two  three", 14)
      assert(key(e, Key.Left(ctrl)).get.caret == 9)
      var back = Editor("one two  three", 14)
      back = key(back, Key.Left(ctrl)).get
      assert(back.caret == 9)
      back = key(back, Key.Left(ctrl)).get
      assert(back.caret == 4)
      back = key(back, Key.Left(ctrl)).get
      assert(back.caret == 0)
      var fwd = Editor("one two  three", 0)
      fwd = key(fwd, Key.Right(ctrl)).get
      assert(fwd.caret == 3)
      fwd = key(fwd, Key.Right(ctrl)).get
      assert(fwd.caret == 7)
      fwd = key(fwd, Key.Right(ctrl)).get
      assert(fwd.caret == 14)
      // Punctuation is not a word char, and never swallowed mid-word.
      val p = Editor("a.b", 0)
      val p1 = key(p, Key.Right(ctrl)).get
      assert(p1.caret == 1)
      assert(key(p1, Key.Right(ctrl)).get.caret == 3)
      assert(key(Editor("a.b", 3), Key.Left(ctrl)).get.caret == 2)
    }

    test("Up and Down move one visual row, preserving the display column") {
      // The prompt wraps rather than truncates, so its motion is over wrapped rows:
      // "abcdefghijklm" at inner width 10 is two rows.
      val e = Editor("abcdefghijklm", 13)
      val up1 = e.up(Box)
      assert(up1.caret == 3)
      // The first visual row with no history is a wall, not an error; down from it
      // still moves within the draft -- row 0 of two is not the last row.
      assert(up1.up(Box).caret == 3)
      assert(up1.up(Box).down(Box).caret == 13)
      // The column is preserved in display columns: after 'c' (column 3) the row
      // above lands at the start of its second 世 (column 3, offset 1).
      val w = Editor("世世世\nabcd", 6)
      assert(w.up(20).caret == 1)
    }

    test("history via up/down: recall from the first row, walk back down to the draft") {
      var e = typed(Editor("", 0), "first").submitted
      e = typed(e, "second").submitted
      e = typed(e, "draft")
      e = e.up(Box)
      assert(e.text == "second")
      e = e.up(Box)
      assert(e.text == "first")
      e = e.up(Box)
      assert(e.text == "first") // the top of history is a wall
      e = e.down(Box)
      assert(e.text == "second")
      e = e.down(Box)
      assert(e.text == "draft" && e.caret == 5 && e.histPos == None)
      // An empty history is a no-op, not an error.
      assert(Editor("", 0).up(Box).text == "")
    }

    test("editing a recalled entry drops back to draft and leaves history intact") {
      var e = typed(Editor("", 0), "first").submitted
      e = typed(e, "live").submitted
      e = e.up(Box)
      assert(e.text == "live")
      e = e.insert("X")
      assert(e.histPos == None)
      assert(e.history == Vector("first", "live"))
      assert(e.down(Box).text == "liveX") // no history position left to walk
    }

    test("the box renders a border, a title when focused, and wrapped content") {
      val e = Editor("hello world", 11, title = "prompt")
      val s = e.render(Size(4, Box))
      assert(s.size == Size(4, 12))
      assert(s.lines(0) == "┌─ prompt ─┐")
      assert(s.lines(1) == "│hello     │")
      assert(s.lines(2) == "│world     │")
      assert(s.lines(3) == "└──────────┘")
      val unfocused = e.copy(focused = false).render(Size(4, Box))
      assert(unfocused.lines(0) == "┌──────────┐")
    }

    test("the box scrolls to keep the caret visible; caretPos is border-adjusted") {
      // Inner width 4 (box 6): "abcdefgh" is two rows, one content row shown.
      val e = Editor("abcdefgh", 8)
      assert(e.render(Size(3, 6)).lines(1) == "│efgh│")
      assert(e.caretPos(6, 3) == Some(grit.tui.model.surface.Pos(1, 5)))
      // A caret after one 世 is two cells in from the border.
      val w = Editor("世x", 1)
      assert(w.caretPos(20, 3) == Some(grit.tui.model.surface.Pos(1, 3)))
    }
  }
}
