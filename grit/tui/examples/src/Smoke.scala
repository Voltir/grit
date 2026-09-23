package grit.tui.examples

import grit.tui.model.surface.Placements
import grit.tui.runtime.*
import grit.tui.model.input.*
import grit.tui.model.surface.*

/** The smallest thing that proves the terminal seam works: take the screen, paint, read
  * real input, resize, and give the terminal back.
  *
  * Not the phase 0 demo -- there is no transcript, no selection, no modal. This exists so
  * `term/` can be verified by painting rather than asserted about, per the scripted-pty
  * recipe in CLAUDE.md.
  *
  * ```./app grit.tui.examples.Smoke```   (Ctrl-Q to quit)
  */
object Smoke {

  final case class State(typed: String, size: Size, resizes: Int, mouse: Option[Pos])

  enum Msg {
    case Typed(ch: Char)
    case Resized(size: Size)
    case Clicked(pos: Pos)
    case Quit
  }

  object SmokeApp extends TuiApp[State, Msg] {

    def init: (State, Effect[Msg]) = (State("", Size(0, 0), 0, None), Effect.NoOp)

    def update: (Msg, State) -> (State, Effect[Msg]) = (msg, state) =>
      msg match {
        case Msg.Typed(c) => (state.copy(typed = (state.typed + c).takeRight(40)), Effect.NoOp)
        case Msg.Resized(s) =>
          (state.copy(size = s, resizes = state.resizes + 1), Effect.Invalidate)
        case Msg.Clicked(p) => (state.copy(mouse = Some(p)), Effect.NoOp)
        case Msg.Quit => (state, Effect.Quit)
      }

    def onInput: (Input, State, Placements) -> Option[Msg] = (input, _, _) =>
      input match {
        case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
        case Input.Keyboard(Key.Printable(c)) => Some(Msg.Typed(c))
        case Input.Resize(s) => Some(Msg.Resized(s))
        // Shift-drag stays unbound: it is the terminal's own selection (rule 9).
        case Input.Mouse(e) if e.mods.shift => None
        case Input.Mouse(e) if e.kind == MouseKind.Press => Some(Msg.Clicked(e.pos))
        case _ => None
      }

    def view: State -> (Size -> Frame) = state =>
      size => {
        val bar = Style(reverse = true)
        var s = Surface.blank(size)
        s = s.fill(Rect(0, 0, 1, math.max(0, size.cols - 1)), Cell(' ', bar))
        s = s.write(0, 1, s" grit.tui smoke -- ${size.rows}x${size.cols} -- ctrl-q to quit ", bar)
        s = s.write(2, 2, "type something; resize me; click me")
        s = s.write(4, 2, s"typed:   ${state.typed}")
        s = s.write(5, 2, s"resizes: ${state.resizes}")
        s = s.write(6, 2, s"mouse:   ${state.mouse.map(p => s"${p.row},${p.col}").getOrElse("-")}")
        // A row across the whole frame: the frame is the paintable screen, so the last
        // column of the terminal is outside it by construction.
        s = s.write(8, 0, "#" * size.cols)
        Frame(s, cursor = None)
      }
  }

  /** `runMain grit.tui.examples.Smoke` needs a `main` here; the app's own is the real one. */
  def main(args: Array[String]): Unit = SmokeApp.main(args)
}
