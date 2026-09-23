package grit.tui.runtime.std

/** The standard app, batteries included: every mixin of the layer, assembled, with the
  * two chains written out.
  *
  * It extends [[grit.tui.runtime.TuiApp]] through [[StdBase]], so an app of this layer names
  * *one* trait and gets `main` with it. What an app supplies is not machinery but
  * *decisions*, each one a hook the mixin that owns it calls at the point Demo2 used to
  * inline it.
  *
  * **This trait is a convenience, not the layer.** The layer is the six traits it mixes,
  * and an app that wants fewer names them instead:
  *
  * {{{
  * object Viewer extends StdBase[State, Msg]
  *     with Ambient[State, Msg] with Selecting[State, Msg] {
  *   val layers = Vector(ambientLayer, pageLayer, pointerLayer)
  *   val steps  = Vector(ambientStep, scrollStep, pointerStep)
  *   // and no onModalClose, no withEditor, no withPopup: it has no modal and no prompt
  * }
  * }}}
  *
  * Those three members are the whole argument for the split. They are abstract, so the
  * indivisible trait made every app answer them -- an app with no modal still had to say
  * what closing its modal meant. A trait an app does not mix asks it nothing.
  *
  * The cost of assembling by hand is that Scala 3 has no way to elide the repeated type
  * arguments, so the mixin list is noisier than one name. That is why this trait exists:
  * an app that wants everything, as Demo2 does, should not pay for the divisibility it
  * is not using.
  *
  * **Precedence is the reason [[layers]] is written out rather than accumulated.** Each
  * mixin exposes its layer and leaves composing to the assembly, because contributing
  * through `super.layers :+ mine` would let the order of the `with` clauses decide
  * routing: hotkeys are bound *before* a modal can capture (quit, the app's modes), free
  * keys only *after* every component has declined (Enter, Tab -- semantics that must not
  * fire while a modal is open). That is the difference between the two, and it is
  * legible here and nowhere else.
  *
  * The machine these hooks serve is Demo2's, unchanged in behavior: a press hit-tests
  * and begins a drag in whichever pane is topmost (rule 6 -- motion never re-hit-tests,
  * it is resolved against the drag's own pane and clamped there); the wheel scrolls
  * whatever pane is under the pointer; a drag parked at a pane's edge autoscrolls one
  * row per tick and re-arms the escaped-drag deadline only while the view is actually
  * moving; a release copies what was selected. What the layer cannot know -- which pane
  * a scrollbar's wheel redirects to, what a submission means, what the status note says
  * -- is exactly what the hooks are for.
  */
trait StdApp[State, Own]
    extends Hotkeys[State, Own]
    with Ambient[State, Own]
    with Selecting[State, Own]
    with Modals[State, Own]
    with Completing[State, Own]
    with FreeKeys[State, Own] {

  /** The routing chain, in the order the layers claim -- the order the nested `onInput`
    * used to hard-code.
    *
    * A `val` and not a `def`, for the reason every hook here is: `onInput` reads it from
    * inside a pure lambda, and a method call there would capture `this`. Which means the
    * layer vals it names must already be initialized when this runs -- they are, because
    * every mixin linearizes before the trait that extends them all.
    */
  val layers: Vector[Layer] = Vector(
    hotkeyLayer,
    ambientLayer,
    modalLayer,
    popupLayer,
    promptLayer,
    freeLayer,
    pageLayer,
    pointerLayer
  )

  /** The update chain. The steps claim disjoint sets of [[Std]] messages, so unlike
    * [[layers]] the order carries no meaning -- but between them they must claim *every*
    * case, and nothing in the compiler says so any more. `StdTests` reads the cases out
    * of `Std.scala` and fails if one is missing.
    */
  val steps: Vector[Step] = Vector(ambientStep, scrollStep, editStep, popupStep, pointerStep)
}
