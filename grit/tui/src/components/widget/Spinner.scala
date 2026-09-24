package grit.tui.components.widget

import grit.tui.components.View
import grit.tui.model.surface.{Cell, Size, Style, Surface}

/** A one-cell progress glyph that cycles through `frames` as `tick` advances.
  *
  * The state is the tick counter, so an idle spinner is simply one that stopped being
  * re-rendered -- and a tick that lands on the same frame produces an identical
  * surface, which the diff painter turns into zero bytes.
  */
final case class Spinner(
    tick: Long,
    frames: Vector[Char] = Spinner.frames,
    style: Style = Style.plain
) extends View {

  /** One cell, whatever it is offered. */
  def measure(avail: Size): Size = Size(math.min(1, avail.rows), math.min(1, avail.cols))

  /** The glyph in the top-left of `size`; the rest is blank. */
  def render(size: Size): Surface =
    Surface
      .blank(size)
      .put(0, 0, Cell(frames(math.floorMod(tick, frames.length).toInt), style))
}

object Spinner {

  /** Braille dot patterns, the conventional terminal spinner. */
  val frames: Vector[Char] = Vector('⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏')
}
