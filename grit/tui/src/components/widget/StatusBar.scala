package grit.tui.components.widget

import grit.tui.components.View
import grit.tui.model.surface.{Cell, Size, Style, Surface}
import grit.tui.model.text.Width

/** A one-row status bar of left- and right-justified segments.
  *
  * Chrome: it truncates, never wraps ([[grit.tui.components.widget.package]]), and the right side
  * yields first -- the left carries the state the user must see, the right is
  * counters. Cuts land in display columns, so a wide glyph is never sliced.
  */
final case class StatusBar(
    left: Vector[String],
    right: Vector[String],
    style: Style = Style(reverse = true)
) extends View {

  /** One row, the full width offered. */
  def measure(avail: Size): Size = Size(math.min(1, avail.rows), avail.cols)

  /** The bar painted across `size`; only the first row is used. */
  def render(size: Size): Surface = {
    val cols = math.max(0, size.cols)
    val surface = Surface.filled(size, Cell(' ', style))
    if (size.rows <= 0) surface
    else {
      val leftCut = Width.fit(left.mkString("  "), cols)
      val withRoom = surface.write(0, 0, leftCut, style)
      val rightBudget = math.max(0, cols - Width.of(leftCut))
      val rightCut = Width.fit(right.mkString("  "), rightBudget)
      withRoom.write(0, math.max(0, cols - Width.of(rightCut)), rightCut, style)
    }
  }
}
