package grit.tui.components.layout

import grit.tui.components.View
import grit.tui.model.surface.{Pos, Rect, Size, Style, Surface}

/** A view wrapped in a frame, with an optional title inset into the top rail.
  *
  * The combinator the four hand-drawn frames became. It measures to its child plus one
  * cell on every side, paints the frame, and blits the child into what is left -- so a
  * child never has to know it is inside anything, and a frame never has to know what it
  * is around.
  */
final case class Box(
    child: View,
    title: String = "",
    border: Border = Border.Round,
    style: Style = Style.plain
) extends View {

  def measure(avail: Size): Size = {
    val inner = child.measure(Size(math.max(0, avail.rows - 2), math.max(0, avail.cols - 2)))
    Size(
      math.min(avail.rows, inner.rows + 2),
      math.min(avail.cols, inner.cols + 2)
    )
  }

  def render(size: Size): Surface = {
    val rect = Rect(0, 0, math.max(0, size.rows), math.max(0, size.cols))
    val framed = Border.draw(Surface.blank(size), rect, border, title, style)
    val inner = rect.inset(1)
    if (inner.rows <= 0 || inner.cols <= 0) framed
    else framed.blit(child.render(inner.size), Pos(inner.top, inner.left))
  }
}
