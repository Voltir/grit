package grit.core.recipe

import grit.core.context.Width
import grit.core.message.Tokens
import grit.core.store.Focus

/** One `A` for each focus a message is said at ([[Focus]]). */
final case class ByFocus[A](focused: A, open: A) {
  def at(focus: Focus): A = focus match {
    case Focus.Focused => focused
    case Focus.Open => open
  }
}

object ByFocus {

  /** `a` at every focus. */
  def both[A](a: A): ByFocus[A] = ByFocus(a, a)
}

/** How a turn is drawn: its window's `width`, and what it is offered. */
final case class Shaping(width: Width, offering: Offering)

/** What a turn answers, as a recipe tells them apart: a heard message at the focus it was
  * said at, or a message said to grit.
  */
enum Rooted {
  case Heard(focus: Focus)
  case Addressed
}

/** How each turn is shaped: one rooted on a heard message by the focus it was said at; one
  * rooted on a message said to grit by `addressed`.
  */
final case class TurnRecipe(heard: ByFocus[Shaping], addressed: Shaping) {

  def at(rooted: Rooted): Shaping = rooted match {
    case Rooted.Heard(focus) => heard.at(focus)
    case Rooted.Addressed => addressed
  }

  /** The widest budget it draws a window at over `window`; `None` when it draws none wider.
    * A window drawn as deployed is never over it.
    */
  def widens(window: Tokens): Option[Tokens] =
    Vector(heard.focused, heard.open, addressed)
      .collect { case Shaping(Width.Within(budget, _), _) => budget }
      .filter(b => Tokens.value(b) > Tokens.value(window))
      .maxByOption(Tokens.value)
}

object TurnRecipe {

  /** Every tool offered at the deployed width, on every turn: what a deployment declaring
    * none runs.
    */
  val Shipped: TurnRecipe = {
    val shaping = Shaping(Width.Deployed, Offering.All)
    TurnRecipe(ByFocus.both(shaping), shaping)
  }
}
