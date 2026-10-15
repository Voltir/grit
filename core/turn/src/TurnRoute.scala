package grit.turn

import grit.core.allot.{Allotment, Headroom, Tasking}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.model.{Grade, Pinned, Pins, TurnProfile}
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
  * ([[grit.core.store.Db.within]]). Recorded as [[Route.Recorded]].
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

  /** What the `route` step records: a [[Route]] with each role's pin named by its place among
    * that role's pins in the turn's profile ([[TurnProfile]]'s `turn`, `summary` and `query`,
    * 0 the first), which `pin-models` recorded by id and the profile store keeps. The rest is
    * what routing decided when it ran: the classification, headroom, task, allotment and
    * baseline. A recorded encoding (ADR 0004) that only grows.
    */
  final case class Recorded(
      classification: Label,
      turn: Int,
      summary: Option[Int],
      query: Option[Int],
      headroom: Headroom,
      task: Task,
      allotment: Allotment,
      baseline: Option[Grade]
  ) {

    /** The route this names among `profile`'s pins; `Left` naming the role whose place
      * `profile` has no pin at.
      */
    def in(profile: TurnProfile): Either[String, Route] = {
      def at(role: String, pins: Pins, place: Int): Either[String, Pinned] =
        pins.all.lift(place).toRight(s"the profile has no $role pin at $place")
      for {
        t <- at("turn", profile.turn, turn)
        s <- summary.fold(Right(None))(at("summary", profile.summary, _).map(Some(_)))
        q <- query.fold(Right(None))(at("query", profile.query, _).map(Some(_)))
      } yield Route(classification, t, s, q, headroom, task, allotment, baseline)
    }
  }

  /** `route` as recorded, each pin by its place among `profile`'s for its role; `Left` naming
    * a role whose pin is none of `profile`'s.
    */
  def recorded(route: Route, profile: TurnProfile): Either[String, Recorded] = {
    def place(role: String, pins: Pins, pin: Pinned): Either[String, Int] =
      Some(pins.all.indexOf(pin)).filter(_ >= 0).toRight(s"the $role pin is not the profile's")
    for {
      t <- place("turn", profile.turn, route.turn)
      s <- route.summary.fold(Right(None))(place("summary", profile.summary, _).map(Some(_)))
      q <- route.query.fold(Right(None))(place("query", profile.query, _).map(Some(_)))
    } yield Recorded(
      route.classification,
      t,
      s,
      q,
      route.headroom,
      route.task,
      route.allotment,
      route.baseline
    )
  }
}
