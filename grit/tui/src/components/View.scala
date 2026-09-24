package grit.tui.components

import grit.tui.model.input.Input
import grit.tui.model.surface.{Rect, Size, Surface}

/** What every component is: something that can say how big it wants to be, paint itself
  * at the size it was given, and say what an input means to it.
  *
  * This is the seam a user of the library implements, and the reason it is one trait
  * rather than a framework. The shape is taken from layoutz's `Element`, whose one
  * virtue -- a single method, so a third-party component composes with every container
  * for free -- is the thing worth copying. Its protocol is not:
  *
  *   - **`Surface`, not `String`.** A component that renders to text cannot report where
  *     it landed, so hit-testing is impossible, and hit-testing is what selection *is*.
  *     Rendering to cells means [[render]] can record placements, so a composed tree
  *     knows where each of its named children ended up.
  *   - **[[measure]] is declared, not derived.** layoutz's `width` renders the element
  *     and measures the result, and is `final`, so every child paints twice a frame and
  *     no component can supply a cheaper answer. Here a parent can afford to ask.
  *   - **The size goes down.** layoutz containers cannot tell a child how much room it
  *     has -- there is one marker node resolved by a parent-side match on one concrete
  *     class -- so every size is intrinsic and "fill what is left" is inexpressible.
  *     Here [[render]] is told exactly what it gets.
  */
trait View {

  /** What this wants, given what is on offer. Never larger than `avail`. */
  def measure(avail: Size): Size

  /** This painted at exactly `size` -- never larger, and never wrapped onto more rows
    * than it was given. Chrome truncates; content that wraps did so in `update`.
    */
  def render(size: Size): Surface

  /** What this component's routing decisions are called.
    *
    * An abstract *type member*, deliberately, and not a shared supertype. `Modal.Route`
    * has no case meaning "give it to the app beneath" and `Popup.Route` does, and that
    * difference is the difference between the two components: a modal that let one key
    * through would not be capturing, and a popup that ate the keystrokes filtering it
    * would be one you could not type into. Nothing here lets a caller treat one as the
    * other, or write a handler that flattens both -- `View` unifies the *shape* of
    * routing (one method, taking an input and the rect this was painted into) and none
    * of its vocabulary.
    */
  type Route

  /** What `input` means to this component, given that it was painted at `at`. */
  def route(input: Input, at: Rect): Route
}

/** A view with no routing of its own: every input goes straight back to the caller.
  *
  * Chrome -- a status bar, a spinner, a scrollbar's rail -- draws and does not bind.
  * Saying so in the type keeps a widget from silently swallowing a key the app meant to
  * bind, which is the failure a default "return None" would hide.
  */
trait Passive extends View {
  type Route = Input
  final def route(input: Input, at: Rect): Input = input
}
