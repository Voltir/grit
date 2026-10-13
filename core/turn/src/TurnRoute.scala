package grit.turn

import grit.core.model.Pinned
import grit.core.visibility.Label

/** What the `route` step decided: the classification the turn was routed for, and the pin
  * each role's calls are made under, the first of its pins cleared for it; `None`: none of
  * that role's is, and its call is not made. A recorded encoding (ADR 0004) that only grows.
  */
final case class Route(
    classification: Label,
    turn: Pinned,
    summary: Option[Pinned],
    query: Option[Pinned]
)
