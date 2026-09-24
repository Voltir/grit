package grit.tui.runtime.loop

import grit.tui.model.input.{Input, MouseKind}
import grit.tui.model.surface.{Frame, Size}
import grit.tui.runtime.app.{App, Effect}
import grit.tui.runtime.render.{Memo, Paint, Painted}
import grit.tui.runtime.route.{Grab, Route, Routed, Tick, Timer}

/** Everything the loop holds between events, as a value: the app's state, what was last
  * painted, the wrap memo, and the pointer. [[Loop]] steps it purely; the runtime only
  * performs what comes back.
  */
final case class Loop[S, M](
    state: S,
    painted: Painted[M],
    memo: Memo,
    grab: Grab,
    stale: Boolean = false
)

object Loop {

  def start[S, M](state: S): Loop[S, M] = Loop(state, Painted.empty[M], Memo.empty, Grab.Idle)

  /** One frame: the tree for this state, laid out and painted at `size`. */
  def paint[S, M <: caps.Pure](
      l: Loop[S, M],
      app: App[S, M],
      size: Size
  ): (Frame, Loop[S, M]) = {
    val (frame, painted, memo) = Paint.frame(app.view(l.state), size, l.memo)
    (frame, l.copy(painted = painted, memo = memo, stale = false))
  }

  /** `l` with its targets laid out for its current state, when a message has changed that
    * state since they were: the handlers a paint captured close over the values it painted
    * (the editor, the scroll position), so routing a second key against them would build
    * on the first key's *input* rather than its result.
    */
  def fresh[S, M <: caps.Pure](l: Loop[S, M], app: App[S, M]): Loop[S, M] =
    if (!l.stale) l
    else {
      val (painted, memo) = Paint.layout(app.view(l.state), l.painted.size, l.memo)
      l.copy(painted = painted, memo = memo, stale = false)
    }

  /** `r`'s messages run through `update` in order, collecting their effects. */
  def applied[S, M <: caps.Pure](
      l: Loop[S, M],
      r: Routed[M],
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) = {
    var s = l.state
    val fx = Vector.newBuilder[Effect[M]]
    var i = 0
    while (i < r.msgs.length) {
      val (next, e) = app.update(r.msgs(i), s)
      s = next
      fx += e
      i += 1
    }
    (l.copy(state = s, grab = r.grab, stale = l.stale || r.msgs.nonEmpty), fx.result(), r.timers)
  }

  /** A terminal input, routed against the last layout and applied. A drag or release
    * under a grab is routed against what was *painted* -- the pane the user sees -- and
    * everything else against a layout of the current state ([[fresh]]).
    */
  def input[S, M <: caps.Pure](
      l: Loop[S, M],
      in: Input,
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) = {
    val grabbed = (l.grab, in) match {
      case (Grab.Idle, _) => false
      case (_, Input.Mouse(e)) => e.kind == MouseKind.Drag || e.kind == MouseKind.Release
      case _ => false
    }
    val at = if (grabbed) l else fresh(l, app)
    applied(at, Route.input(in, at.painted, at.grab), app)
  }

  /** One of the runtime's own ticks. */
  def tick[S, M <: caps.Pure](
      l: Loop[S, M],
      t: Tick,
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) = t match {
    case Tick.Autoscroll => applied(l, Route.autoscroll(l.painted, l.grab), app)
    case Tick.Deadline => applied(l, Route.release(l.painted, l.grab, expired = true), app)
  }

  /** One of the app's own messages. */
  def message[S, M <: caps.Pure](
      l: Loop[S, M],
      m: M,
      app: App[S, M]
  ): (Loop[S, M], Vector[Effect[M]], Vector[Timer]) =
    applied(l, Routed(Vector(m), l.grab, Vector.empty), app)
}
