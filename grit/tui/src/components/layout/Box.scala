package grit.tui.components.layout

import grit.tui.components.View
import grit.tui.model.input.Input
import grit.tui.model.surface.{Pos, Rect, Size, Style, Surface}

/** A view wrapped in a frame, with an optional title inset into the top rail.
  *
  * The combinator the four hand-drawn frames became. It measures to its child plus one
  * cell on every side, paints the frame, and blits the child into what is left -- so a
  * child never has to know it is inside anything, and a frame never has to know what it
  * is around.
  *
  * Input is delegated to the child against the *inset* rect, because the child was
  * painted there: routing a click against the outer rect would put every position one
  * cell out, which is the sort of error that only shows up as a selection that drifts.
  */
final case class Box[C <: View](
    child: C,
    title: String = "",
    border: Border = Border.Round,
    style: Style = Style.plain
) extends View {

  /** The child's own routing vocabulary, unchanged -- a frame is not a decision.
    *
    * `Box` is parameterised on its child's type rather than taking a plain `View`
    * because a `View`-typed field erases which routing vocabulary the child speaks:
    * `Box(popup).Route` would be abstract, and a caller could not match on the
    * `Popup.Route` it obviously is. The type parameter is what keeps the abstract
    * member useful through composition.
    */
  type Route = child.Route

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

  def route(input: Input, at: Rect): Route = child.route(input, at.inset(1))
}
