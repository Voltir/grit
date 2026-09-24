package grit.tui.components.overlay

import grit.tui.components.layout.Border
import grit.tui.model.surface.{Cell, Rect, Size, Style, Surface}

/** A centred dialog over a frozen app: its geometry and its chrome.
  *
  * The modal owns no document. Its body is whatever the tree puts in the rect [[place]]
  * returns -- a document pane, usually -- so scrolling and selection stay in the pane,
  * where rule 6 already lives, and a drag begun in the modal still clamps to the modal's
  * own document.
  *
  * **The app beneath is dimmed, not cleared.** Everything outside the frame keeps its
  * glyphs and gains whatever [[behind]] says -- `dim` alone by default, a darkened ground
  * as well once an app names one: it is frozen, not gone. There is no drop shadow. A
  * shadow drawn as a shade glyph had to `fill`, destroying the app's glyphs to fake depth
  * that a lit [[panel]] over a receding backdrop gives honestly.
  *
  * Capture is not here: it is the tree's. A `dialog` node walls off everything beneath it
  * and ends the focus path, so nothing below sees a key, a press or the wheel, while a
  * hotkey bound outside the dialog still runs.
  */
final case class Modal(
    title: String,
    rows: Int,
    cols: Int,
    panel: Style = Style.plain,
    chrome: Style = Style.plain,
    behind: Style = Style.Dim
) {

  import Modal.*

  /** Where the body goes: centred, shrunk to what the screen can hold around the
    * chrome, or None when the screen is too small for a modal worth painting.
    *
    * This is the *content* rect -- the border is drawn outside it -- so the rect the body
    * is painted into and the pane's own viewport describe the same cells, which is what
    * makes hit-testing invert painting.
    */
  def place(screen: Size): Option[Rect] = {
    if (screen.rows < MinRows || screen.cols < MinCols) None
    else {
      val r = math.min(rows, screen.rows - ReservedRows)
      val c = math.min(cols, screen.cols - ReservedCols)
      if (r <= 0 || c <= 0) None
      else Some(Rect((screen.rows - r) / 2, (screen.cols - c) / 2, r, c).clip(screen))
    }
  }

  /** The bordered rect around a body rect: one cell of frame on every side. */
  def outer(body: Rect): Rect = Rect(body.top - 1, body.left - 1, body.rows + 2, body.cols + 2)

  /** `s` with the modal's chrome drawn around `body`: the app pushed back, and the frame
    * itself. The body is left blank -- in [[panel]] -- for the app to blit into.
    */
  def render(s: Surface, body: Rect): Surface = {
    val o = outer(body)
    frame(backdrop(s, o), o)
  }

  /** Everything outside the frame pushed back under [[behind]] -- glyphs untouched, so
    * the app is still legible behind the dialog it is waiting on.
    */
  private def backdrop(s: Surface, o: Rect): Surface = {
    val size = s.size
    val bands = Vector(
      Rect(0, 0, o.top, size.cols),
      Rect(o.bottom, 0, size.rows - o.bottom, size.cols),
      Rect(o.top, 0, o.rows, o.left),
      Rect(o.top, o.right, o.rows, size.cols - o.right)
    )
    bands.foldLeft(s) { (acc, band) => acc.restyle(band, behind.over) }
  }

  /** The panel, and the frame drawn on it.
    *
    * The rails are drawn in `chrome.over(panel)` rather than in `chrome`: a border is
    * *on* the panel, so a chrome style that names only a foreground keeps the panel's
    * ground under it instead of punching a hole in it.
    */
  private def frame(s: Surface, o: Rect): Surface =
    Border.draw(s.fill(o, Cell(' ', panel)), o, Border.Round, title, chrome.over(panel), TitledFrom)
}

object Modal {

  /** Rows and columns the chrome and its margin claim around the body: two of frame,
    * and enough of the app left showing to read as a backdrop.
    */
  private val ReservedRows = 6
  private val ReservedCols = 10

  /** Below this there is no room for a dialog anyone could use. */
  private val MinRows = 9
  private val MinCols = 24

  /** Narrower than this and the title would be all frame and no title. */
  private val TitledFrom = 8
}
