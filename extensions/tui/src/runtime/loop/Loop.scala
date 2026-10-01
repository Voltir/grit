package grit.tui.runtime.loop

import grit.tui.model.input.{Input, MouseKind}
import grit.tui.model.surface.{Frame, Pos, Rect, Size, Style, Surface}
import grit.tui.runtime.app.{App, Effect, Fault}
import grit.tui.runtime.render.{Memo, Paint, Painted}
import grit.tui.runtime.route.{Grab, Route, Routed, Tick, Timer}

/** Everything the loop holds between events, as a value: the app's state, what was last
  * painted, the wrap memo, and the pointer. [[Loop]] steps it purely; the runtime only
  * performs what comes back.
  *
  * `good` is the last frame the app's view painted, kept so a view that throws leaves it
  * on screen. `fault` is the latest throwable caught in the app ([[Fault]]), drawn over
  * the bottom row until a key is pressed after it was shown (`shown`); `faults` counts
  * every one caught, so the runtime can tell when a new one has come.
  */
final case class Loop[S, M](
    state: S,
    painted: Painted[M],
    memo: Memo,
    grab: Grab,
    stale: Boolean = false,
    good: Option[Frame] = None,
    fault: Option[Fault] = None,
    shown: Boolean = false,
    faults: Long = 0L
) {

  /** This loop with `f` caught. */
  def failed(f: Fault): Loop[S, M] = copy(fault = Some(f), shown = false, faults = faults + 1)
}

object Loop {

  def start[S, M](state: S): Loop[S, M] = Loop(state, Painted.empty[M], Memo.empty, Grab.Idle)

  /** One frame: the tree for this state, laid out and painted at `size`, with the latest
    * fault over its bottom row. A view that throws paints the last good frame instead,
    * and leaves the layout input is routed against as it was.
    */
  def paint[S, M <: caps.Pure](
      l: Loop[S, M],
      app: App[S, M],
      size: Size
  ): (Frame, Loop[S, M]) =
    try {
      val (frame, painted, memo) = Paint.frame(app.view(l.state), size, l.memo)
      val next = l.copy(painted = painted, memo = memo, stale = false, good = Some(frame))
      (next.fault.fold(frame)(banner(frame, _)), next.copy(shown = next.fault.nonEmpty))
    } catch {
      case e: Throwable if Fault.survivable(e) =>
        val f = Fault.of(Fault.Stage.View, e)
        val last = l.good.fold(Frame(Surface.blank(size), None)) { g =>
          if (g.surface.size == size) g
          else Frame(Surface.blank(size).blit(g.surface, Pos(0, 0)), None)
        }
        (banner(last, f), l.failed(f).copy(stale = false, shown = true))
    }

  /** `frame` with `f` said on its bottom row, in reverse video: the library has no
    * palette, and reverse reads on any.
    */
  def banner(frame: Frame, f: Fault): Frame = {
    val s = frame.surface
    if (s.size.rows < 1) frame
    else {
      val r = s.size.rows - 1
      val lit = Style(reverse = true, bold = true)
      val drawn = s.fill(Rect(r, 0, 1, s.size.cols), grit.tui.model.surface.Cell(' ', lit))
      frame.copy(surface = drawn.write(r, 0, Fault.line(f), lit))
    }
  }

  /** `l` with its targets laid out for its current state, when a message has changed that
    * state since they were: the handlers a paint captured close over the values it painted
    * (the editor, the scroll position), so routing a second key against them would build
    * on the first key's *input* rather than its result. A view that throws leaves the
    * last layout in place.
    */
  def fresh[S, M <: caps.Pure](l: Loop[S, M], app: App[S, M]): Loop[S, M] =
    if (!l.stale) l
    else {
      try {
        val (painted, memo) = Paint.layout(app.view(l.state), l.painted.size, l.memo)
        l.copy(painted = painted, memo = memo, stale = false)
      } catch {
        case e: Throwable if Fault.survivable(e) =>
          l.failed(Fault.of(Fault.Stage.View, e)).copy(stale = false)
      }
    }

  /** `r`'s messages run through `update` in order, collecting their effects. A message
    * whose `update` throws is dropped: the state stays as it was before it.
    */
  def applied[S, M <: caps.Pure](
      l: Loop[S, M],
      r: Routed[M],
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) = {
    var at = l
    var s = l.state
    val fx = Vector.newBuilder[Effect[M]]
    var i = 0
    while (i < r.msgs.length) {
      try {
        val (next, e) = app.update(r.msgs(i), s)
        s = next
        fx += e
      } catch {
        case e: Throwable if Fault.survivable(e) => at = at.failed(Fault.of(Fault.Stage.Update, e))
      }
      i += 1
    }
    (at.copy(state = s, grab = r.grab, stale = l.stale || r.msgs.nonEmpty), fx.result(), r.timers)
  }

  /** A terminal input, routed against the last layout and applied. A drag or release
    * under a grab is routed against what was *painted* -- the pane the user sees -- and
    * everything else against a layout of the current state ([[fresh]]). A key dismisses
    * a fault that has been shown; a handler that throws drops the input.
    */
  def input[S, M <: caps.Pure](
      l0: Loop[S, M],
      in: Input,
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) = {
    val l = in match {
      case Input.Keyboard(_) | Input.Paste(_) if l0.shown => l0.copy(fault = None, shown = false)
      case _ => l0
    }
    val grabbed = (l.grab, in) match {
      case (Grab.Idle, _) => false
      case (_, Input.Mouse(e)) => e.kind == MouseKind.Drag || e.kind == MouseKind.Release
      case _ => false
    }
    val at = if (grabbed) l else fresh(l, app)
    val routed =
      try { Right(Route.input(in, at.painted, at.grab)) }
      catch { case e: Throwable if Fault.survivable(e) => Left(Fault.of(Fault.Stage.Handler, e)) }
    routed match {
      case Right(r) => applied(at, r, app)
      case Left(f) => (at.failed(f).copy(grab = Grab.Idle), Vector.empty, Vector.empty)
    }
  }

  /** One of the runtime's own ticks. */
  def tick[S, M <: caps.Pure](
      l: Loop[S, M],
      t: Tick,
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) = {
    val routed =
      try {
        Right(t match {
          case Tick.Autoscroll => Route.autoscroll(l.painted, l.grab)
          case Tick.Deadline => Route.release(l.painted, l.grab, expired = true)
        })
      } catch { case e: Throwable if Fault.survivable(e) => Left(Fault.of(Fault.Stage.Handler, e)) }
    routed match {
      case Right(r) => applied(l, r, app)
      case Left(f) => (l.failed(f).copy(grab = Grab.Idle), Vector.empty, Vector.empty)
    }
  }

  /** One of the app's own messages. */
  def message[S, M <: caps.Pure](
      l: Loop[S, M],
      m: M,
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) =
    applied(l, Routed(Vector(m), l.grab, Vector.empty), app)
}
