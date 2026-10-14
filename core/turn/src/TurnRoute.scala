package grit.turn

import grit.core.allot.{Allotment, Headroom, Tasking}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.model.{Grade, Pinned}
import grit.core.triage.Weighing
import grit.core.visibility.Label

/** What the `route` step decided: the classification the turn was routed for; the pin each
  * role's calls are made under, the turn's chosen among its pins cleared for it, the summary's
  * and query's the first of theirs (`None`: none of that role's is, and its call is not made);
  * the day's `headroom`; what it asked of the turn's root (`task`); and what it lays over the
  * turn's calls (`allotment`), applied wherever the turn makes them; and the room's `baseline` in
  * force when it routed (`None`: none), which `turn` was floored by
  * ([[grit.core.route.Routing.floored]]). Every later read of the
  * turn whose content a model is sent is bounded by `classification`
  * ([[grit.core.store.Db.within]]). A recorded encoding (ADR 0004) that only grows.
  */
final case class Route(
    classification: Label,
    turn: Pinned,
    summary: Option[Pinned],
    query: Option[Pinned],
    headroom: Headroom,
    task: Route.Task,
    allotment: Allotment,
    baseline: Option[Grade]
)

object Route {

  /** What `route` asked of the turn's root. */
  enum Task {

    /** Nothing: the pins it could be routed to were of one grade, or it was routed before
      * tasks were asked.
      */
    case Skipped

    /** Answered; the call's cost is in the ledger under [[id]]. */
    case Read(asked: Tasking.Asked)

    /** Asked and not answered, for `why`. */
    case Unanswered(why: Weighing.Unweighed)
  }

  /** The id the task call's cost is kept under in the usage ledger, recorded in the turn's
    * `offer` step's transaction; no entry has it.
    */
  def id(turn: TurnRef): EntryId = EntryId(s"route:${WorkflowId.value(turn.workflowId)}")
}
