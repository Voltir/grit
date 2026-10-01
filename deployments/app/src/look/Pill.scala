package grit.app.look

import grit.tui.components.view.View
import grit.tui.model.surface.{Size, Style, Surface}
import grit.tui.model.text.Width

/** A tab's pill, `label`, in `theme`: lit on the accent while it is `on`, as the header's
  * name is, and faint otherwise. The same width either way ([[Pill.cols]]), so a row of
  * them does not shift when the selection moves. Its cells' ground is left to what it
  * sits on.
  */
final case class Pill(label: String, on: Boolean, theme: Theme) extends View {

  def measure(avail: Size): Size =
    Size(math.min(1, avail.rows), math.min(Pill.cols(label), avail.cols))

  def render(size: Size): Surface =
    if (size.rows < 1) Surface.blank(size)
    else if (on) {
      val lit = Style.fg(theme.headerFg) + Style.bg(theme.headerBg) + Style.Bold
      val edge = Style.fg(theme.headerBg)
      Surface
        .blank(size)
        .write(0, 0, "▐", edge)
        .write(0, 1, s" $label ", lit)
        .write(0, Width.of(label) + 3, "▌", edge)
    } else Surface.blank(size).write(0, 0, s"  $label  ", Style.fg(theme.faint))
}

object Pill {

  /** The columns a pill labelled `label` takes. */
  def cols(label: String): Int = Width.of(label) + 4
}
