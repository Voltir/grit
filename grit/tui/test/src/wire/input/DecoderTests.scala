package grit.tui.wire.input

import grit.tui.model.input.*
import grit.tui.model.surface.Pos

import utest.*

object DecoderTests extends TestSuite {

  /** Everything `s` decodes to when it arrives as one read, with the tail resolved --
    * the terminal has stopped talking, so a dangling prefix is answered now.
    */
  private def whole(s: String): Vector[Input] = {
    val (out, d) = Decoder.empty.feed(s)
    out ++ d.flush._1
  }

  /** The same bytes delivered one character per read. */
  private def byByte(s: String): Vector[Input] = {
    var d = Decoder.empty
    val out = Vector.newBuilder[Input]
    var i = 0
    while (i < s.length) {
      val (evs, next) = d.feed(s.substring(i, i + 1))
      out ++= evs
      d = next
      i += 1
    }
    out ++= d.flush._1
    out.result()
  }

  /** The same bytes delivered as two reads split at `cut`. */
  private def atCut(s: String, cut: Int): Vector[Input] = {
    val (a, d1) = Decoder.empty.feed(s.substring(0, cut))
    val (b, d2) = d1.feed(s.substring(cut))
    a ++ b ++ d2.flush._1
  }

  /** The oracle. A read boundary is not an event: a script must decode identically
    * however the kernel chose to chop it, including one character at a time.
    *
    * This is what the pushback deque next door existed to fake (FINDINGS 2), and what
    * JLine's blocking reader hid in the sibling. A pure decoder cannot ask for one more
    * character, so carrying the partial sequence is the whole design -- and this sweep
    * is the only thing that proves it carries it correctly.
    */
  private def assertSplitInvariant(s: String): Vector[Input] = {
    val expected = whole(s)
    assert(byByte(s) == expected)
    var cut = 1
    while (cut < s.length) {
      assert(atCut(s, cut) == expected)
      cut += 1
    }
    expected
  }

  private val Esc = "\u001b"

  val tests = Tests {

    test("a printable run decodes to one event per character") {
      assert(
        assertSplitInvariant("hi!") == Vector("h", "i", "!").map(c => key(Key.Printable(c.head)))
      )
    }

    test("control characters that have a name decode to that name, not to Ctrl") {
      // Tab, LF and CR are Ctrl-I, Ctrl-J and Ctrl-M on the wire. A binding table that
      // had to match both spellings would miss one of them.
      assert(whole("\t") == Vector(key(Key.Tab)))
      assert(whole("\r") == Vector(key(Key.Enter)))
      assert(whole("\n") == Vector(key(Key.Enter)))
      assert(whole("\u007f") == Vector(key(Key.Backspace)))
      assert(whole("\b") == Vector(key(Key.Backspace)))
      assert(whole("\u0011") == Vector(key(Key.Ctrl('q'))))
      assert(whole("\u0003") == Vector(key(Key.Ctrl('c'))))
    }

    test("named keys decode from both CSI and SS3 spellings") {
      assert(assertSplitInvariant(s"$Esc[A") == Vector(key(Key.Up())))
      assert(assertSplitInvariant(s"${Esc}OA") == Vector(key(Key.Up())))
      assert(assertSplitInvariant(s"$Esc[3~") == Vector(key(Key.Delete())))
      assert(assertSplitInvariant(s"$Esc[5~") == Vector(key(Key.PageUp())))
      assert(assertSplitInvariant(s"$Esc[Z") == Vector(key(Key.BackTab)))
    }

    test("modifiers ride named keys, so Ctrl-Arrow is bindable") {
      // Open question 6, spent: `ESC[1;5D` used to decode as bare Left, so word-wise
      // motion could not be bound. xterm's code `m` carries shift/alt/ctrl in `m-1`
      // as bits 1/2/4.
      val ctrl = Mods(false, false, true)
      assert(whole(s"$Esc[1;5D") == Vector(key(Key.Left(ctrl))))
      assert(whole(s"$Esc[1;2A") == Vector(key(Key.Up(Mods(true, false, false)))))
      assert(whole(s"$Esc[1;3C") == Vector(key(Key.Right(Mods(false, true, false)))))
      assert(whole(s"$Esc[3;5~") == Vector(key(Key.Delete(ctrl))))
      assert(whole(s"$Esc[1;5H") == Vector(key(Key.Home(ctrl))))
      // SS3 carries no modifier byte; a bare CSI arrow is none.
      assert(whole(s"${Esc}OC") == Vector(key(Key.Right())))
      // and the split-read invariant holds for the modified forms too
      assert(assertSplitInvariant(s"$Esc[1;5D") == Vector(key(Key.Left(ctrl))))
    }

    test("a lone ESC stays pending until the reader says time is up") {
      // The ambiguity JLine answered with setAmbiguousTimeout(80). A pure decoder has no
      // clock, so the timeout is the reader's call, spelled `flush`.
      val (out, d) = Decoder.empty.feed(Esc)
      assert(out.isEmpty)
      val (resolved, cleared) = d.flush
      assert(resolved == Vector(key(Key.Escape)))
      assert(cleared.pendingIsEmpty)
    }

    test("ESC followed by a sequence is that sequence, never Escape plus its bytes") {
      // layoutz's parser got this wrong in the other direction: it answered Escape and
      // leaked the rest as printable characters (FINDINGS 6.1).
      val (out, d) = Decoder.empty.feed(s"$Esc[D")
      assert(out == Vector(key(Key.Left())))
      assert(d.pendingIsEmpty)
    }

    test("ESC with a printable character is Alt") {
      assert(assertSplitInvariant(s"${Esc}b") == Vector(key(Key.Alt('b'))))
    }

    test("a press, a drag and a release decode with 1-based wire coordinates dropped") {
      val script = s"$Esc[<0;21;7M$Esc[<32;21;3M$Esc[<0;21;3m"
      assert(
        assertSplitInvariant(script) == Vector(
          mouse(MouseKind.Press, Button.Left, Pos(6, 20), Mods.none),
          mouse(MouseKind.Drag, Button.Left, Pos(2, 20), Mods.none),
          mouse(MouseKind.Release, Button.Left, Pos(2, 20), Mods.none)
        )
      )
    }

    test("the shift bit survives decoding, so the app can leave shift-drag alone") {
      // Rule 9: shift-drag is the terminal's own selection. The decoder must report it
      // rather than swallow it, or the app cannot decline to bind it.
      val evs = assertSplitInvariant(s"$Esc[<36;10;5M")
      assert(evs == Vector(mouse(MouseKind.Drag, Button.Left, Pos(4, 9), Mods(true, false, false))))
    }

    test("wheel detents decode as wheel, not as button presses") {
      assert(
        assertSplitInvariant(s"$Esc[<64;1;1M$Esc[<65;1;1M") == Vector(
          mouse(MouseKind.Wheel, Button.WheelUp, Pos(0, 0), Mods.none),
          mouse(MouseKind.Wheel, Button.WheelDown, Pos(0, 0), Mods.none)
        )
      )
    }

    test("motion with no button held is Move, not Drag") {
      assert(
        assertSplitInvariant(s"$Esc[<35;4;2M") == Vector(
          mouse(MouseKind.Move, Button.None, Pos(1, 3), Mods.none)
        )
      )
    }

    test("a bracketed paste is one event and its body is never decoded") {
      // The body can contain anything the user copied, escape bytes included. Decoding
      // it would turn a pasted transcript into synthetic keystrokes.
      val body = s"a\r\n$Esc[Bz"
      val script = s"$Esc[200~$body$Esc[201~"
      assert(assertSplitInvariant(script) == Vector(Input.Paste(body)))
    }

    test("focus notifications decode") {
      assert(assertSplitInvariant(s"$Esc[I$Esc[O") == Vector(Input.Focus(true), Input.Focus(false)))
    }

    test("a malformed mouse report is reported raw and does not wedge the decoder") {
      val script = s"$Esc[<0;1M$Esc[<0;2;2M"
      val evs = whole(script)
      assert(evs.length == 2)
      assert(evs(0) == key(Key.Unknown(s"$Esc[<0;1M")))
      assert(evs(1) == mouse(MouseKind.Press, Button.Left, Pos(1, 1), Mods.none))
    }

    test("an unterminated parameter run is dropped rather than buffered forever") {
      // A stream that never sends a final byte must not grow the decoder without bound.
      val junk = s"$Esc[" + "1;" * 200
      val (out, d) = Decoder.empty.feed(junk)
      assert(out.nonEmpty)
      val (after, _) = d.feed(s"$Esc[A")
      assert(after == Vector(key(Key.Up())))
    }

    test("a mixed script decodes identically however the reads are chopped") {
      val script =
        s"hi$Esc[A$Esc[<0;5;5M$Esc[<32;5;1M$Esc[<0;5;1m$Esc[200~pasted$Esc[201~$Esc[3~"
      val evs = assertSplitInvariant(script)
      assert(evs.length == 8)
      assert(evs.last == key(Key.Delete()))
    }
  }

  private def key(k: Key): Input = Input.Keyboard(k)

  private def mouse(kind: MouseKind, button: Button, pos: Pos, mods: Mods): Input =
    Input.Mouse(MouseEvent(kind, button, pos, mods))
}
