package grit.app.look

import grit.tui.components.view.View
import grit.tui.model.surface.{Size, Style, Surface}
import grit.tui.model.text.Width

/** The welcome to a conversation with nothing in it yet, in `theme`: grit's name in runes
  * over a marked rule, the tagline, then `keys` (a key, and what it does). Centred in the
  * box it is given, a little above the middle; a box too short for the keys shows the
  * name and tagline alone. Its cells' ground is left to what it sits on.
  */
final case class Splash(theme: Theme, keys: Vector[(String, String)]) extends View {
  import Splash.*

  def measure(avail: Size): Size = avail

  def render(size: Size): Surface = {
    val head: Vector[Line] = Vector(
      Vector(Look.Runes.Name.mkString(" ") -> (Style.fg(theme.grit) + Style.Bold)),
      Vector(s"${Rule} ${Look.Runes.Turn} ${Rule}" -> Style.fg(theme.rail)),
      Vector(Tagline -> Style.fg(theme.faint))
    )
    val keyCols = keys.map((k, _) => Width.of(k)).maxOption.getOrElse(0)
    val listed: Vector[Line] = keys.map((k, what) =>
      Vector(k.padTo(keyCols, ' ') + "   " -> Style.fg(theme.grit), what -> Style.fg(theme.faint))
    )
    val all = head ++ Option.when(listed.nonEmpty)(Vector.empty[(String, Style)]) ++ listed
    val lines = if (all.length <= size.rows) all else head
    val top = math.max(0, (size.rows - lines.length) * 2 / 5)
    // The keys are one block, centred as a whole, so their columns stay aligned.
    val blockCols = listed.map(width).maxOption.getOrElse(0)
    lines.zipWithIndex.foldLeft(Surface.blank(size)) { case (s, (line, i)) =>
      val cols = if (i < head.length) width(line) else blockCols
      val left = math.max(0, (size.cols - cols) / 2)
      line
        .foldLeft((s, left)) { case ((acc, col), (text, style)) =>
          (acc.write(top + i, col, text, style), col + Width.of(text))
        }
        ._1
    }
  }
}

object Splash {

  /** What grit is, in a line. */
  val Tagline = "memory, not scrollback"

  private type Line = Vector[(String, Style)]

  private val Rule = "─" * 5

  private def width(line: Line): Int = line.map((t, _) => Width.of(t)).sum
}
