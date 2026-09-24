package grit.tui.runtime

import grit.tui.model.input.Input
import grit.tui.model.surface.{Frame, Size}

/** An app run with no terminal, as a value: the loop stepped purely at a fixed `size`,
  * every effect the app asked for kept in order rather than performed, and the screen
  * painted on demand. What a test drives an app through -- terminal input and host
  * messages go through exactly the steps [[Runtime]] takes, so a test reads the
  * painted screen rather than the state.
  *
  * Timers are not run: a scheduled message is only an effect in [[effects]], and the
  * runtime's own timers only entries in [[timers]]. A test that wants one to fire
  * delivers it with [[message]] or [[tick]].
  */
final case class Headless[S, M <: caps.Pure](
    app: App[S, M],
    size: Size,
    loop: Loop[S, M],
    effects: Vector[Effect[M]],
    timers: Vector[Timer] = Vector.empty
) {

  def state: S = loop.state

  /** A terminal input, routed against what is on screen now. */
  def input(in: Input): Headless[S, M] = stepped(Loop.input(loop, in, app))

  /** Each input in turn, with no paint between them: one batch. */
  def inputs(ins: Input*): Headless[S, M] = ins.foldLeft(this)(_.input(_))

  /** A message from outside the app: a host's answer, or a timer that fired. */
  def message(m: M): Headless[S, M] = stepped(Loop.message(loop, m, app))

  /** One of the runtime's own timers, fired. */
  def tick(t: Tick): Headless[S, M] = stepped(Loop.tick(loop, t, app))

  /** The screen as the runtime would paint it now, and the loop with that paint kept. */
  def painted: (Frame, Headless[S, M]) = {
    val (frame, next) = Loop.paint(loop, app, size)
    (frame, copy(loop = next))
  }

  /** `painted`'s screen as text, one string per row. */
  def screen: Vector[String] = painted._1.surface.lines

  /** Painted, so the next input is routed against this state's layout. */
  def repainted: Headless[S, M] = painted._2

  private def stepped(r: (Loop[S, M], Vector[Effect[M]], Vector[Timer])): Headless[S, M] =
    copy(loop = r._1, effects = effects ++ r._2, timers = timers ++ r._3)
}

object Headless {

  /** `app` started at `size`: its `init`, its first effect kept, and its first paint. */
  def start[S, M <: caps.Pure](app: App[S, M], size: Size): Headless[S, M] = {
    val (s0, e0) = app.init
    Headless(app, size, Loop.start[S, M](s0), Vector(e0)).repainted
  }
}
