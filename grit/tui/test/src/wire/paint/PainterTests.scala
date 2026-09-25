package grit.tui.wire.paint

import grit.tui.model.surface.*

import utest.*

/** Painter tests assert against the painted grid -- what a terminal would show -- never
  * against the bytes. Byte-level checks are exactly what missed three of the four
  * FINDINGS bugs. The few byte-level assertions that exist are about the wire itself:
  * no bare LF, balanced sync framing.
  */
object PainterTests extends TestSuite {

  /** Feeds `bytes` through the VT model and asserts the full painted grid: '.' is a
    * blank cell, any other char is that glyph -- every cell of every row.
    */
  private def assertPainted(vt: Vt)(rows: String*): Unit = {
    val expected: Vector[String] = rows.toVector.map(_.map(c => if (c == '.') ' ' else c))
    assert(vt.text == expected)
  }

  private def frameOf(size: Size, f: Surface => Surface, cursor: Option[Pos] = None): Frame =
    Frame(f(Surface.blank(size)), cursor)

  val tests = Tests {

    val size = Size(3, 6)

    test("a full paint paints every cell of every row") {
      val frame =
        frameOf(size, s => s.write(0, 0, "title").write(1, 1, "body").write(2, 2, "sta"))
      val vt = new Vt(3, 6)
      vt.feed(Painter.paint(frame, None))
      assertPainted(vt)(
        "title.",
        ".body.",
        "..sta."
      )
    }

    test("the painter writes the frame's own last column -- rule 2 is not its to enforce") {
      // The frame the runtime hands over is already the paintable screen
      // (Size.screen translated it at the boundary), so the painter writes the full
      // width it is given. A frame that says something in its last column is answered:
      // clipping here as well would silently drop that column from every frame.
      val terminal = Size(3, 7)
      val toTheEdge = frameOf(Size.screen(terminal), s => s.write(1, 0, "abcdef"))
      val vt = new Vt(terminal.rows, terminal.cols)
      vt.feed(Painter.paint(toTheEdge, None))
      assertPainted(vt)(
        ".......",
        "abcdef.",
        "......."
      )
      // What Size.screen is there for: a frame as wide as the terminal writes its last
      // glyph into the terminal's last column, and the row's closing erase takes it back.
      val tooWide = frameOf(terminal, s => s.write(1, 0, "abcdefg"))
      val vt2 = new Vt(terminal.rows, terminal.cols)
      vt2.feed(Painter.paint(tooWide, None))
      assertPainted(vt2)(
        ".......",
        "abcdef.",
        "......."
      )
    }

    test("the VT model defers wrap as xterm does, so it can see rule 2's hazard") {
      // A glyph in the last column leaves the cursor on it with a wrap pending: an erase
      // takes that glyph back, a further glyph wraps first, and a move cancels the wrap.
      val vt = new Vt(3, 4)
      vt.feed("\u001b[1;1Habcd\u001b[K")
      vt.feed("\u001b[2;1Habcd\u001b[2;2Hx")
      assertPainted(vt)(
        "abc.",
        "axcd",
        "...."
      )
      assert(vt.cursor == Pos(1, 2))
      val wrapped = new Vt(3, 4)
      wrapped.feed("\u001b[1;1Habcdef")
      assertPainted(wrapped)(
        "abcd",
        "ef..",
        "...."
      )
    }

    test("the VT model paints a stream cut anywhere as it paints it whole") {
      // A terminal reads in chunks and a sequence can straddle two of them; the model
      // must finish it, not print its tail as glyphs.
      val frame = frameOf(
        size,
        s =>
          s.write(0, 0, "title", Style.fg(Color.hex("#7aa2f7")) + Style.Bold)
            .write(1, 1, "sel", Style(reverse = true))
            .write(2, 0, "status", Style.bg(Color.hex("#24283b")))
      )
      val bytes = Painter.paint(frame, None) + "\u001b[?1049h"
      val whole = new Vt(3, 7)
      whole.feed(bytes)
      var cut = 0
      while (cut <= bytes.length) {
        val split = new Vt(3, 7)
        split.feed(bytes.take(cut))
        split.feed(bytes.drop(cut))
        assert(
          split.cells == whole.cells,
          split.cursor == whole.cursor,
          split.flag("1049") == whole.flag("1049")
        )
        cut += 1
      }
      assert(whole.flag("1049"), whole.text(1) == " sel   ")
    }

    test("reverse video lands on exactly the styled cells") {
      val frame =
        frameOf(size, s => s.write(1, 1, "ab", Style(reverse = true)).write(1, 3, "cd"))
      val vt = new Vt(3, 6)
      vt.feed(Painter.paint(frame, None))
      assert(vt.reverseMask == Vector("......", ".xx...", "......"))
    }

    test("colour survives the round trip through the wire, cell for cell") {
      // The oracle for colour is the same as for everything else: paint it, replay the
      // bytes through the VT model, and compare the *grid*. A style that encodes but
      // decodes to something else is invisible to any check on the encoder alone.
      //
      // The foreground's `38;2;7;2;1` carries a red channel of 7 and a green of 2 -- the
      // codes for reverse and dim. A decoder that walks SGR params one at a time reads
      // them as attributes and desyncs the rest of the sequence.
      val fg = Color(7, 2, 1)
      val bg = Color.hex("#24283b")
      val lit = Style.fg(fg) + Style.bg(bg) + Style.Bold + Style.Italic
      val frame = frameOf(
        size,
        s => s.write(0, 0, "ab", lit).write(1, 2, "cd", Style.fg(fg)).write(2, 4, "e", Style.bg(bg))
      )
      val vt = new Vt(3, 6)
      vt.feed(Painter.paint(frame, None))
      assert(vt.styles == frame.surface.cells.grouped(6).toVector.map(_.map(_.style)))
      assert(vt.fgMask(fg) == Vector("xx....", "..xx..", "......"))
      assert(vt.bgMask(bg) == Vector("xx....", "......", "....x."))
      assert(vt.text == Vector("ab    ", "  cd  ", "    e "))
    }

    test("the output contains no bare line feed, and sync framing is balanced") {
      val frame = frameOf(size, s => s.write(0, 0, "x").write(2, 2, "y"))
      val out = Painter.paint(frame, None)
      assert(!out.contains('\n'))
      assert(out.startsWith("\u001b[?2026h"))
      assert(out.endsWith("\u001b[?2026l"))
      // exactly one of each framing sequence: it opens the output and closes it
      assert(out.indexOf("\u001b[?2026l") == out.length - 8)
    }

    test("adjacent same-style runs coalesce into one CUP and one SGR") {
      val frame = frameOf(
        size,
        s => s.write(0, 0, "ab", Style(reverse = true)).write(0, 2, "cde", Style(reverse = true))
      )
      val out = Painter.paint(frame, None)
      assert(out.split("\u001b\\[0;7m").length - 1 == 1)
      assert(out.split("\u001b\\[1;1H").length - 1 == 1)
    }

    test("diff paint of an unchanged frame writes nothing") {
      val frame = frameOf(size, s => s.write(1, 0, "body"))
      val first = Painter.paint(frame, None)
      val vt = new Vt(3, 6)
      vt.feed(first)
      val again = Painter.paint(frame, Some(frame))
      // Zero bytes, not even the sync wrapper: nothing painted is not a frame.
      assert(again == "")
      vt.feed(again)
      assertPainted(vt)(
        "......",
        "body..",
        "......"
      )
    }

    test("diff paint repaints only the changed run") {
      val before = frameOf(size, s => s.write(1, 0, "aaaaa").write(2, 0, "ccccc"))
      val after = frameOf(size, s => s.write(1, 0, "aaaaa").write(2, 0, "cXccc"))
      val vt = new Vt(3, 6)
      vt.feed(Painter.paint(before, None))
      val out = Painter.paint(after, Some(before))
      // The only move is to the changed cell (1-based), and the only glyph written is it.
      val moves = "\u001b\\[(\\d+);(\\d+)H".r.findAllMatchIn(out).map(_.matched).toVector
      assert(moves == Vector("\u001b[3;2H"))
      assert(out.replaceAll("\u001b\\[[0-9;?]*[A-Za-z]", "") == "X")
      vt.feed(out)
      assertPainted(vt)(
        "......",
        "aaaaa.",
        "cXccc."
      )
    }

    test("diff paint handles text that shrank") {
      val before = frameOf(size, s => s.write(1, 0, "abcdef"))
      val after = frameOf(size, s => s.write(1, 0, "abc"))
      val vt = new Vt(3, 6)
      vt.feed(Painter.paint(before, None))
      vt.feed(Painter.paint(after, Some(before)))
      assertPainted(vt)(
        "......",
        "abc...",
        "......"
      )
    }

    test("the cursor is stated, and not restated when it has not moved") {
      val frame = frameOf(size, s => s.write(0, 0, "hi"), cursor = Some(Pos(2, 3)))
      val out = Painter.paint(frame, None)
      assert(out.contains("\u001b[?25h"))
      assert(out.contains("\u001b[3;4H"))
      val again = Painter.paint(frame, Some(frame))
      assert(!again.contains("25"))
      assert(!again.contains('H'))
    }

    test("a cursor-less frame hides the cursor") {
      val frame = frameOf(size, s => s.write(0, 0, "hi"))
      assert(Painter.paint(frame, None).contains("\u001b[?25l"))
    }

    test("a size change forces a full repaint") {
      val before = frameOf(Size(2, 4), s => s.write(0, 0, "aa"))
      val after = frameOf(Size(3, 6), s => s.write(0, 0, "aa"))
      val out = Painter.paint(after, Some(before))
      val vt = new Vt(3, 6)
      vt.feed(out)
      assertPainted(vt)(
        "aa....",
        "......",
        "......"
      )
    }

    test("the terminal's owed column takes the ground of the cell before it, with no glyph") {
      // The frame is the paintable screen, one column narrower than the terminal (rule
      // 2). The column it leaves out is never written, but it must not keep the
      // terminal's own background: an app with a ground would show a stripe there.
      val ground = Color.hex("#1e2030")
      val slab = Color.hex("#2a2f45")
      val frame = frameOf(
        size,
        s =>
          s.fill(Rect(0, 0, 3, 6), Cell(' ', Style.bg(ground)))
            .write(1, 0, "abcdef", Style.bg(slab) + Style.fg(Color.White))
      )
      val vt = new Vt(3, 7)
      vt.feed(Painter.paint(frame, None))
      assert(vt.text.map(_.last) == Vector(' ', ' ', ' '))
      assert(
        vt.cells.map(_.last.style) == Vector(Style.bg(ground), Style.bg(slab), Style.bg(ground))
      )
      assert(vt.text(1).take(6) == "abcdef")
    }

    test("a resize leaves no stale glyph or colour in the owed column") {
      // A terminal keeps what the owed column held when it is resized under the app;
      // the full repaint after a resize must clear it.
      val vt = new Vt(3, 7)
      vt.feed("\u001b[2;7Hz")
      val ground = Color.hex("#1e2030")
      val frame = frameOf(size, s => s.fill(Rect(0, 0, 3, 6), Cell(' ', Style.bg(ground))))
      vt.feed(Painter.paint(frame, Some(frameOf(Size(3, 5), identity))))
      assert(vt.text.map(_.last) == Vector(' ', ' ', ' '))
      assert(vt.bgMask(ground) == Vector.fill(3)("xxxxxxx"))
    }

    test("a diff that leaves a row's last cell alone erases nothing after its run") {
      // The row's closing erase belongs to a run that reaches the last cell. One after a
      // run that stops short would take back the glyphs beyond it and the owed column.
      val ground = Color.hex("#1e2030")
      val before = frameOf(
        size,
        s =>
          s.fill(Rect(0, 0, 3, 6), Cell(' ', Style.bg(ground)))
            .write(0, 2, "cdef", Style.bg(ground))
      )
      val after = Frame(before.surface.write(0, 0, "ab", Style.bg(ground)), None)
      val vt = new Vt(3, 7)
      vt.feed(Painter.paint(before, None))
      val out = Painter.paint(after, Some(before))
      vt.feed(out)
      assert(!out.contains("\u001b[K"))
      assertPainted(vt)(
        "abcdef.",
        ".......",
        "......."
      )
      assert(vt.bgMask(ground) == Vector.fill(3)("xxxxxxx"))
    }
  }
}
