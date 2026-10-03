package grit.core.period

import grit.core.id.{
  CloseRef,
  PeriodRef,
  SettleRef,
  ShadowRef,
  StitchRef,
  TriageRef,
  TurnRef,
  TurnSeq,
  WorkflowId
}

/** A closed period whose raw entries are still kept, its turns running `first` to `last`. */
final case class Purgeable(period: PeriodRef, first: TurnSeq, last: TurnSeq) {

  /** Its turns' workflows, which its purge deletes. */
  def turns: Vector[WorkflowId] =
    (TurnSeq.value(first) to TurnSeq.value(last)).toVector
      .map(t => TurnRef(period.conversationId, TurnSeq(t)).workflowId)

  /** What the ids of the other workflows its purge deletes start with: every attempt made
    * to close it, every question asked about it, every triage and shadow of a message heard
    * in it, and every placement ([[StitchRef.Prefix]]), of which only those [[holds]] are
    * its own.
    */
  def attempts: Vector[String] =
    Vector(
      CloseRef.prefix(period),
      SettleRef.prefix(period),
      TriageRef.prefix(period),
      ShadowRef.prefix(period),
      StitchRef.Prefix
    )

  /** Whether the workflow `id`, found by [[attempts]], is one its purge deletes: a placement
    * only when it is of one of its turns; every other id [[attempts]] finds.
    */
  def holds(id: WorkflowId): Boolean =
    StitchRef.fromWorkflowId(id) match {
      case Some(s) =>
        s.turn.conversationId == period.conversationId &&
        TurnSeq.value(s.turn.turnSeq) >= TurnSeq.value(first) &&
        TurnSeq.value(s.turn.turnSeq) <= TurnSeq.value(last)
      case None => !WorkflowId.value(id).startsWith(StitchRef.Prefix)
    }
}
