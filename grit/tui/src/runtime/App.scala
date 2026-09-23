package grit.tui.runtime

import grit.tui.model.input.Input
import grit.tui.model.surface.{Frame, Placements, Size}

/** What an app is: four pure functions and its own two types.
  *
  * Not a widget framework and not a class hierarchy. grit.tui supplies the surface algebra,
  * the decoder and the runtime; the app supplies these.
  *
  * Every member here is referentially transparent and none takes a capability, which is
  * the whole design: state changes are values, and the things that touch the world come
  * back as [[Effect]] data for the runtime -- which *does* hold the capabilities -- to
  * interpret. And the purity is a fact about the **types**, not about a review: [[update]]
  * and [[onInput]] are declared as `A -> B` function values, the capture-checked pure
  * function type -- `A => B` is `(A -> B)^{cap}`, a function that may capture anything --
  * so an implementation whose lambda reached for a terminal or a scheduler is a compile
  * error. A method declaration would not say this: methods never capture directly, so a
  * capability referenced in a method body folds silently into the enclosing object. The
  * same rule the `StdApp` hooks follow, applied to the core seam they were taken from.
  */
trait App[State, Msg] {

  /** The starting state and whatever should happen immediately. */
  def init: (State, Effect[Msg])

  /** The next state, and what the runtime should do besides changing it. */
  def update: (Msg, State) -> (State, Effect[Msg])

  /** What the screen should look like: a pure `state -> size -> frame` function, called
    * once per repaint, so it must be O(viewport): wrapping belongs in [[update]],
    * threading a cache.
    *
    * It is curried so an app can name the size-independent part of its view once per
    * state and be left with a `Size -> Frame` -- the shape the Snapshot path and the
    * runtime's repaint both consume.
    *
    * `size` is already the paintable screen ([[grit.tui.model.surface.Size.screen]]): one
    * column narrower than the terminal, because the last column is owed back (rule 2).
    * The runtime translates it at the boundary, so the frame an app builds is exactly
    * what may be written and nothing else compensates.
    */
  def view: State -> (Size -> Frame)

  /** What an input *means* in this state, or `None` to ignore it.
    *
    * Binding lives here rather than in the library because it is inseparable from the
    * app's own messages and from the state the event arrives in -- a modal swallows
    * nearly everything. Leaving shift-drag unbound is an app's decision to make, and the
    * decoder reports the modifier precisely so it can be made.
    *
    * `at` is where the last painted frame put every named thing. Without it an app has
    * to keep a parallel map of rects in its own state and refresh it on every change
    * that could move something -- derived state that goes stale silently the first time
    * a `copy` forgets. The runtime already knows, because it just painted the frame:
    * this is the same rule a pane has always followed (input maps back through the last
    * `Viewport` it produced), applied to everything.
    *
    * It is empty before the first paint. The startup `Resize` arrives then, which is
    * correct -- learning how big the screen is cannot depend on having already drawn it.
    */
  def onInput: (Input, State, Placements) -> Option[Msg]
}
