package grit.tui.wire.paint

import grit.tui.model.surface.Style

/** Escape-sequence builders: pure string functions, the only knowledge of terminal
  * grammar outside `term/`. Rows and columns are 1-based, as the terminal counts them.
  */
object Ansi {

  private val Esc = "\u001b["

  /** Absolute cursor addressing: `ESC[<row>;<col>H`. Never a bare line feed -- see
    * README rule 1.
    */
  def cup(row: Int, col: Int): String = s"$Esc${row};${col}H"

  /** SGR to make the terminal show `target`, given the style it already shows
    * (`current`, None = unknown). None when nothing needs to be sent; otherwise a
    * reset-anchored sequence (`ESC[0;...m`), so an emission never depends on what came
    * before it -- which is also why no `39`/`49` default-colour codes are ever needed.
    *
    * Attributes are emitted before colours. Nothing downstream depends on the order,
    * but keeping it fixed means a style's encoding is a function of the style alone.
    */
  def sgr(target: Style, current: Option[Style]): Option[String] = {
    if (current.contains(target)) None
    else if (target == Style.plain) Some(s"${Esc}0m")
    else {
      val attrs = Vector(
        if (target.bold) Some("1") else None,
        if (target.dim) Some("2") else None,
        if (target.italic) Some("3") else None,
        if (target.underline) Some("4") else None,
        if (target.reverse) Some("7") else None,
        target.fg.map(c => s"38;2;${c.r};${c.g};${c.b}"),
        target.bg.map(c => s"48;2;${c.r};${c.g};${c.b}")
      ).flatten.mkString(";")
      Some(s"${Esc}0;${attrs}m")
    }
  }

  /** Erase from the cursor to the end of the line, in the current background: `ESC[K`.
    * Moves nothing and writes no glyph, so it is how the column rule 2 never writes is
    * still coloured.
    */
  val eraseLine: String = s"${Esc}K"

  val reset: String = s"${Esc}0m"
  val syncStart: String = s"$Esc?2026h"
  val syncEnd: String = s"$Esc?2026l"
  val cursorShow: String = s"$Esc?25h"
  val cursorHide: String = s"$Esc?25l"
}
