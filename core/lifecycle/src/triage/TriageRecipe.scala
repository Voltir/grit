package grit.lifecycle.triage

import grit.core.recipe.Pool
import grit.core.store.Focus

/** What triage's question shows besides the message, its author and its thread: the pool for a
  * message said where its container is the topic (`focused`), and the one for a message said
  * where topics interleave (`open`).
  */
final case class TriageRecipe(focused: Pool, open: Pool) {

  /** The pool for a message said at `focus`. */
  def at(focus: Focus): Pool = focus match {
    case Focus.Focused => focused
    case Focus.Open => open
  }
}

object TriageRecipe {

  /** What triage ships with: no pool at either focus. */
  val Shipped: TriageRecipe = TriageRecipe(Pool.empty, Pool.empty)
}
