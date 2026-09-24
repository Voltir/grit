package grit.tui.runtime

import grit.tui.model.surface.{Frame, Size}
import grit.tui.wire.paint.{Ansi, Painter}
import grit.tui.wire.term.{Stdout, SystemTerminal}

/** An [[App]] that can be run: it supplies `main`, so an app is its four pure functions
  * and nothing else.
  *
  * What this removes is not typing but a place to get it wrong. Opening the terminal,
  * detecting that there isn't one, creating the scheduler and tearing down in the right
  * order was ~15 lines copied verbatim into every example -- and the copies had already
  * drifted into closing the scheduler twice, harmless only because `close` is
  * idempotent. Ordering is now stated once, in [[Runtime.run]], where the resources are.
  */
trait TuiApp[State, Msg] extends App[State, Msg] {

  /** The size the non-tty path renders at, when there is no terminal to ask. Given to
    * [[firstFrame]] already translated to the paintable screen, exactly as the runtime
    * translates the terminal's size.
    */
  def fallbackSize: Size = Size(24, 80)

  /** One frame from the initial state, for the non-tty path.
    *
    * Overridable because an app whose first frame depends on derived layout has to
    * compute it before painting -- `view` is not allowed to (rule 7).
    */
  def firstFrame(size: Size): Frame = {
    val (state, _) = init
    view(state)(size)
  }

  def main(args: Array[String]): Unit = {
    val _ = args
    TuiApp.run(this)
  }
}

object TuiApp {

  /** Run `app` on the system terminal, or print one frame if there isn't one.
    *
    * The non-tty branch is what makes `./app` pipeable and what `./gate` leans on: the
    * same pure `view`, painted once, with no modes taken. Detection is `isatty(1)` and
    * not a heuristic, which is why it can be trusted to decide this.
    */
  def run[State, Msg](app: TuiApp[State, Msg]^): Unit = run(app, Host.none[Msg])

  /** As [[run]], with `host` receiving the app's [[Effect.ToHost]] messages. */
  def run[State, Msg](app: TuiApp[State, Msg]^, host: Host[Msg]^): Unit =
    SystemTerminal.open() match {
      case None =>
        // The non-tty path answers to the same rule the runtime does: the frame is the
        // paintable screen, one column narrower than the size it is asked for.
        Stdout.emit(
          Painter.paint(app.firstFrame(Size.screen(app.fallbackSize)), None) + Ansi.reset + "\n"
        )
      case Some(term) =>
        // The scheduler is handed over, not held: `Runtime.run` registers it for
        // release, so there is exactly one owner and exactly one close.
        new Runtime(app, term, Scheduler.create(), host).run()
    }
}
