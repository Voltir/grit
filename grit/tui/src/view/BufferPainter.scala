package grit.tui.view

case class Viewport(cols: Int, rows: Int)

trait Sink {
  def write(s: String): Unit
  def flush(): Unit
}

/** Measures how many terminal cells a string occupies.
  *
  * Explicit because the answer is not `String.length`: CJK characters take two cells and
  * combining marks take none, and getting it wrong makes the pinned input region drift
  * down the screen one row at a time.
  */
trait TextWidth {
  def width(s: String): Int
}

object TextWidth {

  /** One cell per code point. Wrong for CJK; adequate for tests over ASCII. */
  object Monospace extends TextWidth {
    def width(s: String): Int = s.codePointCount(0, s.length)
  }
}

/** Puts a `Screen` on the terminal, rewriting only the tail that changed.
  *
  * This is pi's renderer, rebuilt: one unbounded buffer, a viewport that is always its
  * tail, and two paths.
  *
  *   - **Patch.** The first changed line is still on screen, so the cursor moves there
  *     *relatively* and everything from there to the end is rewritten. Rows past the old
  *     bottom are created by writing a newline, which scrolls the terminal — and that is
  *     what hands the displaced lines to native scrollback, as real content rather than as
  *     a repaint artefact.
  *   - **Reprint.** The change landed above the viewport, or the width moved, so no
  *     relative move can reach it. Erase screen *and scrollback*, then write the whole
  *     buffer again. Erasing the scrollback is not a loss of history: the reprint
  *     regenerates it immediately, reflowed to the new width. Skipping the erase is what
  *     leaves a terminal with a scrollback full of duplicate transcripts.
  *
  * Why not JLine's own `Display`: it caps its work at `numLines = min(rows, …)`
  * (`Display.java:533`), so a buffer taller than the screen is either silently truncated
  * — measured: the footer simply never drew — or, if told the region is as tall as the
  * buffer, repaints past the bottom and piles duplicates into the scrollback. It is the
  * right tool for a bounded region and the wrong one for a transcript.
  *
  * **All bookkeeping is in buffer *rows*, never line indices and never screen rows.** A
  * line that wraps occupies several rows, so line indices do not measure cursor motion; and
  * screen rows shift under you every time the terminal scrolls. Buffer rows are the only
  * coordinate that is stable across both, which is why `viewportTop` is the only conversion
  * in the file.
  */
final class BufferPainter(sink: Sink, sizeOf: () => Viewport, tw: TextWidth) {

  import BufferPainter.*

  private var painted: Vector[String] = Vector.empty
  private var paintedWidth: Int = -1
  private var viewportTop: Int = 0
  private var cursorRow: Int = 0

  /** One cell short of the terminal.
    *
    * A line that exactly fills the width leaves the cursor somewhere the rewind arithmetic
    * cannot predict — some terminals wrap at once, others defer — so the last column is
    * never used.
    */
  def viewport(): Viewport = {
    val vp = sizeOf()
    Viewport(math.max(20, vp.cols - 1), math.max(3, vp.rows))
  }

  def paint(screen: Screen): Unit = {
    val vp = viewport()
    val lines = screen.lines
    val out = new StringBuilder

    out ++= SyncStart
    out ++= HideCursor

    val changed = firstChange(painted, lines)
    val startRow = changed.map(i => rowsOf(lines.take(i), vp.cols))

    val mustReprint =
      paintedWidth != vp.cols || painted.isEmpty || startRow.exists(_ < viewportTop)

    if (mustReprint) { reprint(out, lines, vp) }
    else { changed.foreach(from => patch(out, lines, vp, from)) }

    placeCaret(out, screen.caret, lines, vp)

    out ++= ShowCursor
    out ++= SyncEnd
    painted = lines
    paintedWidth = vp.cols
    sink.write(out.toString)
    sink.flush()
  }

  /** Index of the first line that differs, or `None` when the buffers match. */
  private def firstChange(old: Vector[String], next: Vector[String]): Option[Int] = {
    val shared = math.min(old.length, next.length)
    var i = 0
    while (i < shared && old(i) == next(i)) { i += 1 }
    if (i < shared) Some(i)
    else if (old.length != next.length) Some(shared)
    else None
  }

  // TODO: RM — duplicate. Same fold as Layout.rowsOf (Page.scala), which Screen.of uses.
  // Keep one, wherever the wrapping code ends up living.
  private def rowsOf(lines: Vector[String], cols: Int): Int =
    lines.foldLeft(0)((acc, l) => acc + Frame.rows(l, cols, tw))

  private def reprint(out: StringBuilder, lines: Vector[String], vp: Viewport): Unit = {
    out ++= ClearAll
    writeFrom(out, lines, 0)
    settle(out, lines, vp, clearedTo = 0)
  }

  private def patch(out: StringBuilder, lines: Vector[String], vp: Viewport, from: Int): Unit = {
    /* Lines before `from` are identical in both buffers, so their combined height is the
     * same and this row is valid in either coordinate system. */
    val startRow = rowsOf(lines.take(from), vp.cols)
    moveToRow(out, startRow)
    writeFrom(out, lines, from)
    settle(out, lines, vp, clearedTo = rowsOf(painted, vp.cols))
  }

  /** Write `lines` from index `from` to the end, leaving the cursor on the final row. */
  private def writeFrom(out: StringBuilder, lines: Vector[String], from: Int): Unit = {
    var i = from
    while (i < lines.length) {
      if (i > from) { out ++= "\r\n" }
      out ++= "\r"
      out ++= ClearToEol
      out ++= lines(i)
      i += 1
    }
  }

  /** Clear whatever the previous, taller buffer left below, then record where we are. */
  private def settle(
      out: StringBuilder,
      lines: Vector[String],
      vp: Viewport,
      clearedTo: Int
  ): Unit = {
    val total = rowsOf(lines, vp.cols)
    val leftover = math.min(clearedTo - total, vp.rows)
    if (leftover > 0) {
      var n = 0
      while (n < leftover) { out ++= "\r\n"; out ++= ClearToEol; n += 1 }
      out ++= up(leftover)
    }
    cursorRow = math.max(0, total - 1)
    viewportTop = math.max(0, total - vp.rows)
  }

  private def placeCaret(
      out: StringBuilder,
      caret: Caret,
      lines: Vector[String],
      vp: Viewport
  ): Unit = {
    val line = math.max(0, math.min(caret.line, lines.length - 1))
    val cols = math.max(1, vp.cols)
    val row = rowsOf(lines.take(line), cols) + (caret.col / cols)
    moveToRow(out, math.max(viewportTop, row))
    out ++= "\r"
    val col = caret.col % cols
    if (col > 0) { out ++= right(col) }
  }

  /** Relative move between two buffer rows. Valid only while both are on screen, which is
    * what the `startRow < viewportTop` reprint guard is for.
    */
  private def moveToRow(out: StringBuilder, target: Int): Unit = {
    val delta = target - cursorRow
    if (delta > 0) { out ++= down(delta) }
    else if (delta < 0) { out ++= up(-delta) }
    cursorRow = target
  }

  /** Forget the screen and the scrollback — the `/clear` command. */
  def reset(): Unit = {
    painted = Vector.empty
    paintedWidth = -1
    viewportTop = 0
    cursorRow = 0
    sink.write(ClearAll)
    sink.flush()
  }

  /** Leave the cursor below the buffer so a shell prompt starts on a clean line. */
  def finish(): Unit = {
    sink.write(ShowCursor + "\r\n")
    sink.flush()
  }
}

object BufferPainter {

  private val Esc: String = 27.toChar.toString

  val HideCursor: String = Esc + "[?25l"
  val ShowCursor: String = Esc + "[?25h"
  val ClearToEol: String = Esc + "[K"

  /** Erase screen, home, erase scrollback — always followed by a full reprint. */
  val ClearAll: String = Esc + "[2J" + Esc + "[H" + Esc + "[3J"

  /** Synchronized output. The terminal buffers everything between these and presents the
    * frame atomically, so a multi-row patch is never seen half-drawn. Terminals that do not
    * know mode 2026 ignore both.
    */
  val SyncStart: String = Esc + "[?2026h"
  val SyncEnd: String = Esc + "[?2026l"

  def up(n: Int): String = Esc + "[" + n.toString + "A"
  def down(n: Int): String = Esc + "[" + n.toString + "B"
  def right(n: Int): String = Esc + "[" + n.toString + "C"
}
