package grit.tui.examples

import grit.tui.components.View
import grit.tui.model.input.{Input, Key}
import grit.tui.model.surface.*
import grit.tui.components.{Node, OnInput}
import grit.tui.runtime.{App, Effect}

/** The smallest thing that proves the terminal seam works: take the screen, paint, read
  * real input, resize, and give the terminal back.
  *
  * There is no transcript, no selection, no modal. This exists so `wire.term` can be
  * verified by painting rather than asserted about, per the scripted-pty recipe in
  * CLAUDE.md.
  *
  * ```./mill --no-daemon --no-build-lock grit.tui.examples.runMain grit.tui.examples.Smoke```
  * (Ctrl-Q to quit)
  */
object Smoke extends App[Smoke.State, Smoke.Msg] {

  final case class State(typed: String, mouse: Option[Pos])

  enum Msg extends caps.Pure {
    case Typed(ch: Char)
    case Clicked(pos: Pos)
    case Quit
  }

  def init: (State, Effect[Msg]) = (State("", None), Effect.NoOp)

  def update(msg: Msg, state: State): (State, Effect[Msg]) =
    msg match {
      case Msg.Typed(c) => (state.copy(typed = (state.typed + c).takeRight(40)), Effect.NoOp)
      case Msg.Clicked(p) => (state.copy(mouse = Some(p)), Effect.NoOp)
      case Msg.Quit => (state, Effect.Quit)
    }

  private def keys: OnInput[Msg] = {
    case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
    case Input.Keyboard(Key.Printable(c)) => Some(Msg.Typed(c))
    case _ => None
  }

  def view(state: State): Node[Msg] =
    Node.paint(Card(state)).onPress(p => Some(Msg.Clicked(p))).onKey(keys)

  /** The whole screen. Painted at whatever size it is given, so a resize shows up as the
    * size in the title bar.
    */
  private final case class Card(state: State) extends View {
    def measure(avail: Size): Size = avail

    def render(size: Size): Surface = {
      val bar = Style(reverse = true)
      var s = Surface.blank(size)
      s = s.fill(Rect(0, 0, 1, size.cols), Cell(' ', bar))
      s = s.write(0, 1, s" grit.tui smoke -- ${size.rows}x${size.cols} -- ctrl-q to quit ", bar)
      s = s.write(2, 2, "type something; resize me; click me")
      s = s.write(4, 2, s"typed:   ${state.typed}")
      s = s.write(5, 2, s"mouse:   ${state.mouse.map(p => s"${p.row},${p.col}").getOrElse("-")}")
      // A row across the whole frame: the frame is the paintable screen, so the last
      // column of the terminal is outside it by construction.
      s.write(7, 0, "#" * size.cols)
    }
  }
}
