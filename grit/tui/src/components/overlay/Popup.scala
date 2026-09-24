package grit.tui.components.overlay

import grit.tui.components.layout.Border
import grit.tui.components.view.View
import grit.tui.model.input.{Button, Input, Key, MouseKind}
import grit.tui.model.surface.{Pos, Rect, Size, Style, Surface}
import grit.tui.model.text.Width

/** A bounded floating list, anchored to something else on the screen: the completion
  * popup a slash command opens above the prompt.
  *
  * The state is the candidates, the query they are filtered by, and which match is
  * selected. The window over the matches is *derived* rather than stored -- there is no
  * scroll offset to fall out of step with the selection, which is the bug a stored one
  * has: the list scrolls because the selection moved, never the other way round.
  *
  * Where [[Modal]] captures everything, this passes most things on: typing belongs to the
  * editor underneath, and [[Route.Pass]] says so. A popup that ate the keystrokes that
  * filter it would be a popup you could not type into. It claims exactly the keys that
  * mean *this list* -- the arrows, Enter, Tab, Escape -- and the mouse.
  */
final case class Popup(
    items: Vector[String],
    query: String = "",
    selected: Int = 0,
    maxRows: Int = 6,
    maxCols: Int = 32,
    item: Style = Style.plain,
    selectedStyle: Style = Style.Reverse,
    chrome: Style = Style.plain
) extends View {

  /** The box this wants: its widest match, bounded, plus the frame. */
  def measure(avail: Size): Size =
    Size(math.min(avail.rows, boxSize.rows), math.min(avail.cols, boxSize.cols))

  import Popup.*

  /** The candidates the query admits, in the order they were given. Prefix matching,
    * case-insensitively: a completion list answers what you have typed so far.
    */
  def matches: Vector[String] = {
    val q = query.toLowerCase
    items.filter(_.toLowerCase.startsWith(q))
  }

  def isEmpty: Boolean = matches.isEmpty

  /** The selection, clamped into the matches. */
  private def sel: Int = math.max(0, math.min(selected, matches.length - 1))

  /** The same popup filtered by `q`, the selection clamped into what is left. */
  def withQuery(q: String): Popup = {
    val next = copy(query = q)
    next.copy(selected = math.max(0, math.min(selected, next.matches.length - 1)))
  }

  /** `delta` rows down the list, wrapping at both ends -- a completion list is a ring. */
  def move(delta: Int): Popup =
    if (matches.isEmpty) this
    else copy(selected = math.floorMod(sel + delta, matches.length))

  /** What Enter would accept. */
  def choice: Option[String] = if (matches.isEmpty) None else Some(matches(sel))

  /** Content rows shown at once: as many as there are matches, up to `maxRows`. */
  def rows: Int = math.min(matches.length, math.max(0, maxRows))

  /** First visible match. Derived from the selection, so the selection is always in
    * the window by construction.
    */
  def top: Int = math.max(0, sel - rows + 1)

  /** The matches the window shows. */
  def visible: Vector[String] = matches.slice(top, top + rows)

  /** The selected match's row within [[visible]]. */
  def selectedRow: Int = sel - top

  /** The box the list wants: the widest match plus a frame, bounded by `maxCols`. */
  def boxSize: Size = {
    val widest = matches.foldLeft(0)((w, m) => math.max(w, Width.of(m)))
    Size(rows + 2, math.min(math.max(0, maxCols), widest) + 2)
  }

  /** Where the box goes: above `anchor` and left-aligned with it, flipped below when
    * there is no room above, and clamped into the screen. None when nothing matches --
    * an empty completion list is not a small one, it is absent.
    */
  def place(anchor: Rect, screen: Size): Option[Rect] = {
    if (matches.isEmpty) None
    else {
      val b = boxSize
      val h = math.min(b.rows, screen.rows)
      val w = math.min(b.cols, screen.cols)
      val above = anchor.top - h
      val top = if (above >= 0) above else math.max(0, math.min(anchor.bottom, screen.rows - h))
      val left = math.max(0, math.min(anchor.left, screen.cols - w))
      Some(Rect(top, left, h, w).clip(screen))
    }
  }

  /** The list as a `size`-sized surface: a frame, one match per row, the selection in
    * reverse video. Chrome truncates -- a match too wide for the box is cut in display
    * columns, never wrapped onto a second row.
    */
  def render(size: Size): Surface = {
    val h = math.max(0, size.rows)
    val w = math.max(0, size.cols)
    val sided =
      Border.draw(Surface.blank(Size(h, w)), Rect(0, 0, h, w), Border.Round, "", chrome)
    val shown = visible
    shown.indices.foldLeft(sided) { (s, i) =>
      val row = 1 + i
      if (row >= h - 1) s
      else {
        val style = if (i == selectedRow) selectedStyle else item
        val text = Width.fit(shown(i), w - 2)
        val padded = text + " " * math.max(0, (w - 2) - Width.of(text))
        s.write(row, 1, padded, style)
      }
    }
  }

  /** The match under a screen position, if the position is on a list row. */
  def matchAt(box: Rect, pos: Pos): Option[String] = {
    val row = pos.row - box.top - 1
    if (!box.contains(pos) || row < 0 || row >= visible.length) None else Some(visible(row))
  }

  /** What the app must do with one input while the popup is open. */
  def route(input: Input, box: Rect): Popup.Route = input match {
    case Input.Keyboard(Key.Up(_)) => Route.Stay(move(-1))
    case Input.Keyboard(Key.Down(_)) => Route.Stay(move(1))
    case Input.Keyboard(Key.Enter) => accept
    case Input.Keyboard(Key.Tab) => accept
    case Input.Keyboard(Key.Escape) => Route.Dismissed
    case Input.Mouse(e) if e.kind == MouseKind.Wheel =>
      Route.Stay(move(if (e.button == Button.WheelUp) -1 else 1))
    case Input.Mouse(e) if e.kind == MouseKind.Press =>
      matchAt(box, e.pos) match {
        case Some(item) => Route.Chose(item)
        // A click on the frame is a click on the popup: it keeps it, it does not choose.
        case None => if (box.contains(e.pos)) Route.Stay(this) else Route.Dismissed
      }
    case _ => Route.Pass(input)
  }

  private def accept: Route = choice match {
    case Some(item) => Route.Chose(item)
    case None => Route.Dismissed
  }
}

object Popup {

  def of(items: String*): Popup = Popup(items.toVector)

  /** Where one input goes while the popup is open. It has a pass-through case, and that
    * is the difference from a modal: a modal freezes what is behind it, a popup floats
    * over something still being typed into.
    */
  enum Route {

    /** Consumed; this is the popup now. */
    case Stay(popup: Popup)

    /** Accepted: the app should complete with `item` and close the popup. */
    case Chose(item: String)

    /** Escape, or a click outside: the app should close the popup, unchanged. */
    case Dismissed

    /** Not the popup's -- the editor beneath should have it. */
    case Pass(input: Input)
  }
}
