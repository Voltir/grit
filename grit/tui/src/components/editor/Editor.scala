package grit.tui.components.editor

import grit.tui.components.layout.Border
import grit.tui.components.view.View
import grit.tui.model.input.{Input, Key, Mods}
import grit.tui.model.surface.{Pos, Rect, Size, Style, Surface}
import grit.tui.model.text.{Row, Width, Wrap}

/** The prompt editor: multi-line, wrapping, with a caret and a history.
  *
  * The state is the draft plus where history is being walked; every editing method is
  * a pure function to the next state, and [[apply]] routes a decoded [[Input]] to
  * them, returning `None` for anything the *app* owns -- Enter (submit), Tab
  * (completion), Ctrl-C (interrupt), mouse, focus. Binding stays out of the library
  * (the `input/` decision); the editor owns only what it means to edit text.
  *
  * Motion is over **visual rows**: the prompt is the one piece of chrome that wraps
  * rather than truncates, so Up/Down move one wrapped row and preserve the display
  * column through [[Width]]. Up from the first visual row recalls history; Down from
  * the last returns to it, past the recalled entries to the draft you left. Editing a
  * recalled entry drops you back into draft mode -- history is never rewritten.
  *
  * Newlines arrive by paste only: Enter is submit, and no terminal sends a
  * distinguishable newline key. [[measure]] reports the height the draft wants, which
  * a `Region.Fit` grows the box to and a `Region.Fixed` ignores; either way [[render]]
  * paints exactly the box it is handed. [[render]] draws that bordered box -- a title when
  * focused, none otherwise -- and [[caretPos]] gives the box-local caret for the
  * app to place in its `Frame`.
  *
  * A caret is a UTF-16 char offset into `text`, the same per-Char world as the cell
  * grid; display columns are always computed through [[Width]].
  */
final case class Editor(
    text: String,
    caret: Int,
    focused: Boolean = true,
    title: String = "",
    history: Vector[String] = Vector.empty,
    histPos: Option[Int] = None,
    saved: String = "",
    body: Style = Style.plain,
    chrome: Style = Style.plain,
    titleStyle: Style = Style.plain
) extends View {

  /** The border plus one row per wrapped row of draft, and never more than offered.
    *
    * This used to report all the room it was given, on the grounds that a prompt which
    * shrank to its draft would move every time you typed. What makes growth safe now is
    * that the editor no longer decides it: `Region.Fit` does, the app declares it, and
    * a prompt in a `Region.Fixed` still paints in exactly the box it always did. All
    * this says is how tall the draft is.
    *
    * The row count comes from [[visualRows]], the same wrap [[render]] and [[caretPos]]
    * scroll through -- there is no second answer to how tall a draft is, so there is
    * nothing to drift.
    */
  def measure(avail: Size): Size =
    Size(math.min(avail.rows, visualRows(avail.cols).length + 2), avail.cols)

  /** The caret, clamped into the text. */
  private def at: Int = math.max(0, math.min(caret, text.length))

  /** Bounds of the logical line containing `at`: `(start, endBeforeNewline)`. */
  private def lineBounds: (Int, Int) = {
    val c = at
    val start = text.lastIndexOf('\n', math.min(c - 1, text.length - 1)) + 1
    val nl = text.indexOf('\n', c)
    val end = if (nl < 0) text.length else nl
    (start, end)
  }

  /** Insert `s` at the caret, normalizing the \r\n a terminal pastes. */
  def insert(s: String): Editor = {
    val body = s.replace("\r\n", "\n").replace('\r', '\n')
    val c = at
    copy(
      text = text.substring(0, c) + body + text.substring(c),
      caret = c + body.length,
      histPos = None
    )
  }

  def backspace(): Editor = {
    val c = at
    if (c == 0) this
    else copy(text = text.substring(0, c - 1) + text.substring(c), caret = c - 1, histPos = None)
  }

  def delete(): Editor = {
    val c = at
    if (c >= text.length) this
    else copy(text = text.substring(0, c) + text.substring(c + 1), histPos = None)
  }

  def left(): Editor = if (at == 0) this else copy(caret = at - 1)

  def right(): Editor = if (at >= text.length) this else copy(caret = at + 1)

  def home(): Editor = copy(caret = lineBounds._1)

  def end(): Editor = copy(caret = lineBounds._2)

  /** Readline word motion: word chars are letters, digits and `_`. Forward lands at
    * the end of the next word; backward at the start of the current or previous one.
    */
  def wordRight(): Editor = {
    def isWord(c: Char) = c.isLetterOrDigit || c == '_'
    var i = at
    val before = i
    while (i < text.length && isWord(text.charAt(i))) i += 1
    if (i == before) {
      while (i < text.length && !isWord(text.charAt(i))) i += 1
      while (i < text.length && isWord(text.charAt(i))) i += 1
    }
    copy(caret = i)
  }

  def wordLeft(): Editor = {
    def isWord(c: Char) = c.isLetterOrDigit || c == '_'
    var i = at
    val before = i
    while (i > 0 && isWord(text.charAt(i - 1))) i -= 1
    if (i == before) {
      while (i > 0 && !isWord(text.charAt(i - 1))) i -= 1
      while (i > 0 && isWord(text.charAt(i - 1))) i -= 1
    }
    copy(caret = i)
  }

  /** One visual row up; from the first visual row, one entry back in history. */
  def up(boxWidth: Int): Editor = {
    val rows = visualRows(boxWidth)
    rowOf(rows) match {
      case Some(k) if k > 0 => moveTo(rows, k - 1)
      case _ => recallUp
    }
  }

  /** One visual row down; from the last, forward through history and back to the draft. */
  def down(boxWidth: Int): Editor = {
    val rows = visualRows(boxWidth)
    rowOf(rows) match {
      case Some(k) if k < rows.length - 1 => moveTo(rows, k + 1)
      case _ => recallDown
    }
  }

  /** To the visual row `k`, keeping the caret's display column. */
  private def moveTo(rows: Vector[Row], k: Int): Editor =
    (rowOf(rows).flatMap(rows.lift), rows.lift(k)) match {
      case (Some(cur), Some(r)) =>
        val col = Width.columnAtOffset(cur.text, at - cur.startOffset)
        copy(caret = r.startOffset + Width.offsetAtColumn(r.text, math.min(col, Width.of(r.text))))
      case _ => this
    }

  private def recallUp: Editor = {
    if (history.isEmpty) this
    else {
      val pos = histPos match {
        case None => history.length - 1
        case Some(p) => math.max(0, p - 1)
      }
      copy(
        text = history(pos),
        caret = history(pos).length,
        histPos = Some(pos),
        saved = if (histPos.isEmpty) text else saved
      )
    }
  }

  private def recallDown: Editor = histPos match {
    case None => this
    case Some(p) if p + 1 < history.length =>
      copy(text = history(p + 1), caret = history(p + 1).length, histPos = Some(p + 1))
    case Some(_) => copy(text = saved, caret = saved.length, histPos = None, saved = "")
  }

  /** Wrap the whole draft at the box's inner width; rows carry global offsets. */
  private def visualRows(boxWidth: Int): Vector[Row] =
    Wrap.wrap(text, math.max(1, boxWidth - 2))

  /** The visual row containing the caret: the first whose span reaches it, so a caret
    * sitting on a line break or dropped space belongs to the row above.
    */
  private def rowOf(rows: Vector[Row]): Option[Int] = {
    var i = 0
    var found = -1
    while (i < rows.length && found < 0) {
      val r = rows(i)
      if (r.startOffset <= at && at <= r.startOffset + r.text.length) found = i
      i += 1
    }
    if (found >= 0) Some(found) else rows.indices.lastOption
  }

  /** What the app's submit does: the draft into history, a clean prompt. */
  def submitted: Editor =
    copy(text = "", caret = 0, history = history :+ text, histPos = None, saved = "")

  /** Consume an input, or `None` -- the app's to bind. `boxWidth` is the width the
    * box renders at; wrapping needs it.
    */
  def apply(input: Input, boxWidth: Int): Option[Editor] = input match {
    case Input.Keyboard(Key.Printable(c)) => Some(insert(c.toString))
    case Input.Keyboard(Key.Ctrl('a')) => Some(home())
    case Input.Keyboard(Key.Ctrl('e')) => Some(end())
    case Input.Keyboard(Key.Backspace) => Some(backspace())
    case Input.Keyboard(Key.Delete(Mods.none)) => Some(delete())
    case Input.Keyboard(Key.Left(Mods.none)) => Some(left())
    case Input.Keyboard(Key.Right(Mods.none)) => Some(right())
    case Input.Keyboard(Key.Home(Mods.none)) => Some(home())
    case Input.Keyboard(Key.End(Mods.none)) => Some(end())
    case Input.Keyboard(Key.Left(m)) if m.ctrl => Some(wordLeft())
    case Input.Keyboard(Key.Right(m)) if m.ctrl => Some(wordRight())
    case Input.Keyboard(Key.Up(Mods.none)) => Some(up(boxWidth))
    case Input.Keyboard(Key.Down(Mods.none)) => Some(down(boxWidth))
    case Input.Paste(body) => Some(insert(body))
    case _ => None
  }

  /** The bordered box at `boxWidth` x `boxHeight`: title on the top border when
    * focused, wrapped content scrolled to keep the caret visible.
    */
  def render(size: Size): Surface = {
    val w = math.max(0, size.cols)
    val h = math.max(0, size.rows)
    val framed = Border.draw(
      Surface.blank(Size(h, w)),
      Rect(0, 0, h, w),
      Border.Plain,
      if (focused) title else "",
      chrome,
      Border.TitledFrom,
      Some(titleStyle)
    )
    val rows = visibleRows(w, h)
    rows.indices.foldLeft(framed) { (s, i) => s.write(1 + i, 1, rows(i).text, body) }
  }

  /** The content rows the window shows, caret kept in view with minimal scroll. */
  private def visibleRows(boxWidth: Int, boxHeight: Int): Vector[Row] = {
    val innerH = math.max(0, boxHeight - 2)
    val all = visualRows(boxWidth)
    if (innerH == 0) Vector.empty
    else {
      val caretRow = rowOf(all).getOrElse(0)
      val start = math.max(0, math.min(caretRow - innerH + 1, math.max(0, all.length - innerH)))
      val start2 = if (caretRow < start) caretRow else start
      all.slice(start2, start2 + innerH)
    }
  }

  /** The caret as a box-local position for the app's `Frame.cursor`; None when the
    * box has no content area.
    */
  def caretPos(boxWidth: Int, boxHeight: Int): Option[Pos] = {
    if (boxHeight < 3 || boxWidth < 3) return None
    val rows = visualRows(boxWidth)
    rowOf(rows).map { k =>
      val r = rows(k)
      val col = Width.columnAtOffset(r.text, at - r.startOffset)
      val innerH = boxHeight - 2
      val start = math.max(0, math.min(k - innerH + 1, math.max(0, rows.length - innerH)))
      val start2 = if (k < start) k else start
      Pos(1 + (k - start2), 1 + col)
    }
  }
}
