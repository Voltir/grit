package grit.tui.runtime

import grit.tui.components.Node

import grit.tui.model.input.{Input, MouseKind}
import grit.tui.model.surface.{Frame, Size}

/** An app is three pure functions: its start, its transitions, and its screen as one tree.
  *
  * No layout, no wrap cache, no placement map, no hook: the runtime lays the tree out when
  * it paints, keeps what it painted, and routes input against it through the handlers the
  * tree carries.
  *
  * The purity is a fact about the type: the self type `App[S, M]^{}` says an app
  * *object* captures nothing, so an app whose members reach a terminal, a scheduler or a
  * global capability -- through a field, a helper, a mixin or a `using` parameter -- is
  * rejected where it is defined. That is why the members can be plain methods, and why a
  * handler built inside one is a pure `->` with no ceremony.
  *
  * `M <: caps.Pure` closes the other way in: a message delivered by a [[Host]] that
  * carried a capability would hand `update` one. Declaring the message type pure (`enum
  * Msg extends caps.Pure`) makes a capability-typed case a compile error where it is
  * declared. What neither catches: an untracked Java effect, and mutable state
  * (separation checking is off in `grit.tui`).
  */
trait App[S, M <: caps.Pure] { self: App[S, M]^{} =>
  def init: (S, Effect[M])
  def update(msg: M, state: S): (S, Effect[M])
  def view(state: S): Node[M]

  /** The size the non-tty path paints at. */
  def fallbackSize: Size = Size(24, 80)

  def main(args: Array[String]): Unit = {
    val _ = args
    Runtime.run(this, Host.none[M])
  }
}

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
