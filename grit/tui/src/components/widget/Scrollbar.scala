package grit.tui.components.widget

import grit.tui.components.view.View
import grit.tui.model.surface.{Cell, Size, Style, Surface}

/** A one-column scrollbar with a proportional thumb.
  *
  * The state is the geometry -- `content` rows of document, a `window` of them on
  * screen, `offset` rows from the top -- clamped into the document, so a stale offset
  * after a shrink cannot point outside it. `thumb` is where the block lands;
  * `offsetAtRow` is the drag: hit-test the pane the app blitted this under, take the
  * pane-local row, and scroll there. Identity is the placement map
  * ([[grit.tui.components.widget.package]]), not a widget framework.
  */
final case class Scrollbar(
    content: Int,
    window: Int,
    offset: Int,
    railStyle: Style = Style.plain,
    thumbStyle: Style = Style.plain
) extends View {

  /** One column, as tall as it is offered. */
  def measure(avail: Size): Size = Size(avail.rows, math.min(1, avail.cols))

  /** Rows of document beyond the window. */
  private def span: Int = math.max(0, content - window)

  /** The offset, clamped into the document. */
  private def pos: Int = math.max(0, math.min(offset, span))

  /** The thumb as `(startRow, length)` in `track` rows. Always at least one row long
    * (a thumb the eye cannot grab is no thumb), pinned to both ends of its travel.
    */
  def thumb(track: Int): (Int, Int) = {
    val t = math.max(0, track)
    if (span <= 0) (0, t) // the whole document fits: all thumb
    else {
      val len = math.max(1, math.min(t, t * window / content))
      val travel = t - len
      val start = if (travel <= 0) 0 else pos * travel / span
      (start, len)
    }
  }

  /** The document offset to scroll to when the thumb is grabbed at pane-local
    * `row` of a `track`-row scrollbar. Endpoints exact, monotone in the row; rows
    * past the travel clamp to the end of the document.
    */
  def offsetAtRow(track: Int, row: Int): Int = {
    val (_, len) = thumb(track)
    val travel = track - len
    if (travel <= 0) 0
    else {
      val r = math.max(0, math.min(row, travel))
      r * span / travel
    }
  }

  /** The scrollbar painted down `size`: a rail with a thumb block, in column 0. */
  def render(size: Size): Surface = {
    val rows = math.max(0, size.rows)
    val (start, len) = thumb(rows)
    val rail =
      if (size.cols <= 0) Surface.blank(size)
      else
        Surface
          .blank(size)
          .fill(
            grit.tui.model.surface.Rect(0, 0, rows, 1),
            Cell('│', railStyle)
          )
    (start until math.min(rows, start + len)).foldLeft(rail) { (s, row) =>
      s.put(row, 0, Cell('█', thumbStyle))
    }
  }
}
