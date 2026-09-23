package grit.tui.runtime.std

import grit.tui.runtime.{Effect, TuiApp}
import grit.tui.components.pane.Panes
import grit.tui.model.input.Input
import grit.tui.model.surface.Placements

/** The seam every part of the standard layer is written against: where the state lives,
  * and the two chains that run it.
  *
  * This trait holds no opinion about modals, prompts, scrolling or the pointer -- those
  * are the mixins beside it, and [[StdApp]] is the assembly of all of them. What is here
  * is only what none of them could work without: how to reach the panes and the machine's
  * own fragment inside the app's `State`, how to reach the app's own `update`, and the
  * two `Vector`s that say *which* participants exist and in what order.
  *
  * **Both directions are a chain of values.** `onInput` walks [[layers]] until one claims
  * an input; `update` walks [[steps]] until one claims a [[Std]] message. That symmetry
  * is not decoration -- it is what makes the layer divisible at all. A mixin cannot own a
  * hook unless it also owns the message that reads it: `Std.EditTo`'s handler calls
  * `withEditor`, so the trait that supplies `withEditor` has to be the trait that answers
  * `EditTo`. One exhaustive match over `Std` would have kept every hook in one trait.
  *
  * The price is stated plainly: a `Vector[Step]` is not checked for exhaustiveness the
  * way that match was, and a `Std` message nothing claims becomes a silent no-op. What
  * stands in for the compiler is a test that reads the cases out of `Std.scala` and fails
  * if the default `steps` do not name each one.
  *
  * **The vectors are written out, never accumulated.** A mixin exposes its layer and its
  * step as values and leaves the composing to whoever assembles the app. Contributing
  * through `super.layers :+ mine` would have made trait linearization -- the order of the
  * `with` clauses -- decide routing precedence, and precedence here is load-bearing:
  * hotkeys are bound before a modal can capture, free keys only after every component
  * has declined. An order that important is written down in one place where it can be
  * read.
  *
  * Every hook is a **pure function of its arguments** -- the type is `A -> B`, the
  * capture-checked pure function type, not `A => B` (which is `(A -> B)^{cap}`). And each
  * is a **`val`**, not a `def`: a `def` member is a method, and calling a method from
  * inside a pure lambda captures `this`, so the machine's own dispatch could not be typed
  * `A -> B` if it reached its hooks through method calls. That applies to [[layers]] and
  * [[steps]] too -- `onInput` reads `layers` from inside a pure lambda.
  */
trait StdBase[State, Own] extends TuiApp[State, Std | Own] {

  /* ---- state access ------------------------------------------------------------- */

  /** The panes inside `state` -- what every scroll, anchor and pointer step reads. */
  val panes: State -> Panes

  /** `state` with `panes` in place of its own. */
  val withPanes: (State, Panes) -> State

  /** The [[Std.State]] fragment inside `state`. */
  val std: State -> Std.State

  /** `state` with the machine's fragment replaced wholesale. */
  val withStd: (State, Std.State) -> State

  /* ---- the app's own half -------------------------------------------------------- */

  /** What one of the app's own messages means. The machine never calls this except to
    * delegate -- a `Std` message never reaches it, and an `Own` message never reaches
    * the machine.
    */
  protected val ownUpdate: (Own, State) -> (State, Effect[Std | Own])

  /* ---- the two chains ------------------------------------------------------------ */

  /** One link of the routing chain: what this claimant does with an input, given the
    * state and where the last frame put things.
    */
  type Layer = (Input, State, Placements) -> Claim[Std | Own]

  /** One link of the update chain: what this participant does with a [[Std]] message, or
    * `None` for a message that is somebody else's.
    */
  type Step = (Std, State) -> Option[(State, Effect[Std | Own])]

  /** The routing chain, in the order the layers claim. **A composition point**: an app
    * that wants a second overlay, a different precedence, or no prompt at all writes the
    * layers it wants, and every layer its mixins declare is a value it can reuse.
    */
  val layers: Vector[Layer]

  /** The update chain. The order matters far less than [[layers]]' does -- the steps
    * claim disjoint sets of messages -- but it is written out for the same reason: so
    * that what an app handles is one readable list rather than a linearization.
    */
  val steps: Vector[Step]

  /** An optional message as a claim: `None` from a machine default means the input was
    * consumed and meant nothing, not that the app should get a second look at it.
    */
  protected val claimed: Option[Std | Own] -> Claim[Std | Own] = {
    case Some(msg) => Claim.Handled(msg)
    case None => Claim.Swallowed
  }

  /* ---- the machine --------------------------------------------------------------- */

  /** The routing chain walked until something claims. An input nothing claims is
    * nobody's.
    *
    * Indexed rather than written with `collectFirst` or a `LazyList`: iterator-producing
    * combinators are what capture checking rejects here.
    */
  final val onInput: (Input, State, Placements) -> Option[Std | Own] = (input, state, at) => {
    val chain = layers
    var i = 0
    var current = input
    var out: Option[Std | Own] = None
    var settled = false
    while (i < chain.length && !settled) {
      chain(i)(current, state, at) match {
        case Claim.Handled(msg) => out = Some(msg); settled = true
        case Claim.Swallowed => settled = true
        case Claim.Pass(next) => current = next; i += 1
      }
    }
    out
  }

  /** The app's layout, re-run against the screen the layer remembered. Identity here,
    * because a `StdBase` alone does not know what the app lays out; [[Ambient]] wires it
    * to `onResize`, which is that function and has always been required.
    *
    * It runs after [[ownUpdate]] and nowhere else, and that asymmetry is the whole
    * design. A pane is laid out at a size, and since `Region.Fit` that size depends on
    * app state -- so anything that changes the state can change it. For the layer's own
    * messages the layer knows which ones can: `Std.EditTo` and its siblings re-lay out
    * through `onPrompt`, `Std.Resized` through `onResize`. An `Own` message is opaque,
    * so the only safe assumption is that it changed something. Submitting a long draft
    * is exactly that case, and it was a real bug: the prompt collapses from twelve rows
    * to three, the body grows into what it left, and a body pane still holding its old
    * viewport paints blank rows into the bottom of the new one.
    *
    * Running it on *every* message instead was measured and rejected. A drag arrives as
    * a burst of `Std.Pointer`, and re-laying out on each took the per-event cost from
    * ~30us to ~317us -- three times the cost of the single frame the burst now paints.
    * The layer already knows what its own messages do; paying to rediscover it on the
    * one hot path in the library is the wrong trade.
    *
    * The contract this puts on `onResize` is that it is a *function*, not an event
    * handler: given a state and a size it lays that state out, and it may be called
    * again with the same size. [[Ambient]] wires it here for that reason.
    */
  val relayout: State -> State = s => s

  final val update: (Std | Own, State) -> (State, Effect[Std | Own]) = (msg, state) =>
    msg match {
      case s: Std => stdUpdate(s, state)
      // The union is exhausted by the first case: whatever is not `Std` is the app's.
      // `Own` is a type parameter, so the compiler cannot narrow the fallback for us and
      // the cast is that exhaustiveness said at the value level.
      case _ =>
        val (next, effect) = ownUpdate(msg.asInstanceOf[Own], state)
        (relayout(next), effect)
    }

  /** The update chain walked until a step claims. A message no step claims changes
    * nothing -- the honest behaviour for a mixin the app chose not to have.
    */
  private val stdUpdate: (Std, State) -> (State, Effect[Std | Own]) = (s, state) => {
    val chain = steps
    var i = 0
    var out: Option[(State, Effect[Std | Own])] = None
    while (i < chain.length && out.isEmpty) {
      out = chain(i)(s, state)
      i += 1
    }
    out.getOrElse((state, Effect.NoOp))
  }
}
