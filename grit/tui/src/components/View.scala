package grit.tui.components

import grit.tui.model.surface.{Size, Surface}

/** What every leaf component is: something that can say how big it wants to be, and
  * paint itself at the size it was given. What an input means is not here: handlers live
  * in the screen's tree (`Node`), next to what they handle.
  *
  * This is the seam a user of the library implements, and the reason it is one trait
  * rather than a framework. The shape is taken from layoutz's `Element`, whose one
  * virtue -- a single method, so a third-party component composes with every container
  * for free -- is the thing worth copying. Its protocol is not:
  *
  *   - **`Surface`, not `String`.** A component that renders to text has no per-cell
  *     identity, so it cannot be diffed, and nothing painted can be mapped back to what
  *     it showed -- which is what hit-testing, and so selection, is.
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
    * than it was given. Chrome truncates; a document's wrapping is the runtime's.
    */
  def render(size: Size): Surface
}
