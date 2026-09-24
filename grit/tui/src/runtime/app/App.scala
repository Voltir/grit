package grit.tui.runtime.app

import grit.tui.components.tree.Node
import grit.tui.model.surface.Size

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
}
