package grit.tui.components.layout

import grit.tui.model.surface.{Cell, Rect, Style, Surface}
import grit.tui.model.text.Width

/** The six glyphs a frame is drawn from.
  *
  * A value rather than a subtype, so a caller can supply their own set without the
  * closed-match problem next door: layoutz dispatches borders through a `HasBorder`
  * typeclass whose `Styled`/`Colored` instances hand-unroll a match over four hard-coded
  * types, and a third-party bordered component silently loses its border under
  * `.color(...)`. Here there is nothing to fall through.
  */
final case class Border(
    topLeft: Char,
    topRight: Char,
    bottomLeft: Char,
    bottomRight: Char,
    horizontal: Char,
    vertical: Char
)

object Border {

  val Round: Border = Border('╭', '╮', '╰', '╯', '─', '│')
  val Plain: Border = Border('┌', '┐', '└', '┘', '─', '│')
  val Double: Border = Border('╔', '╗', '╚', '╝', '═', '║')
  val Thick: Border = Border('┏', '┓', '┗', '┛', '━', '┃')

  /** Below this many columns a titled frame is drawn plain: `X─ t ─X` needs five cells
    * of chrome before a title has anywhere to go, and a one-character title in a
    * six-column frame reads as damage rather than as a label.
    */
  val TitledFrom: Int = 6

  /** `rect` outlined on `s`, with `title` inset into the top rail when there is room.
    *
    * The one implementation of the chrome that `Editor`, `Modal`, `Popup` and the
    * `Snapshot` example each used to draw for themselves. The title is cut in display
    * columns, so a wide glyph is never split and the rail never grows past `rect.cols`
    * -- a frame that grew because its title was CJK would push everything beside it off
    * the screen.
    */
  def draw(
      s: Surface,
      rect: Rect,
      border: Border = Round,
      title: String = "",
      style: Style = Style.plain,
      titledFrom: Int = TitledFrom,
      titleStyle: Option[Style] = None
  ): Surface = {
    if (rect.rows <= 0 || rect.cols < 2) s
    else {
      val rail0 = s.write(rect.top, rect.left, top(rect.cols, border, title, titledFrom), style)
      // The title is written as part of the rail and then restyled in place: one string
      // builds the top edge, so the title's columns are known rather than re-measured.
      val withTop = titleStyle match {
        case Some(ts) if title.nonEmpty && rect.cols >= titledFrom =>
          val shown = Width.of(Width.fit(title, rect.cols - 5))
          if (shown <= 0) rail0
          else rail0.restyle(Rect(rect.top, rect.left + 3, 1, shown), ts.over)
        case _ => rail0
      }
      val withBottom =
        if (rect.rows < 2) withTop
        else {
          val rail = border.horizontal.toString * (rect.cols - 2)
          withTop.write(
            rect.bottom - 1,
            rect.left,
            s"${border.bottomLeft}$rail${border.bottomRight}",
            style
          )
        }
      (rect.top + 1 until rect.bottom - 1).foldLeft(withBottom) { (acc, row) =>
        acc
          .put(row, rect.left, Cell(border.vertical, style))
          .put(row, rect.right - 1, Cell(border.vertical, style))
      }
    }
  }

  /** `╭─ title ───╮`, or a plain rail when there is no title or no room for one. */
  private def top(cols: Int, border: Border, title: String, titledFrom: Int): String = {
    val rail = border.horizontal.toString * (cols - 2)
    if (title.isEmpty || cols < titledFrom) s"${border.topLeft}$rail${border.topRight}"
    else {
      // "X- " + title + " " + fill + "X": five cells of chrome, the rest is the title's.
      val budget = cols - 5
      val t = Width.fit(title, budget)
      val pad = border.horizontal.toString * math.max(0, budget - Width.of(t))
      s"${border.topLeft}${border.horizontal} $t $pad${border.topRight}"
    }
  }
}
