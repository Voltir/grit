package grit.core.period

import grit.core.id.{
  CloseRef,
  PeriodRef,
  SettleRef,
  ShadowRef,
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
    * to close it, every question asked about it, and every triage and shadow of a message
    * heard in it.
    */
  def attempts: Vector[String] =
    Vector(
      CloseRef.prefix(period),
      SettleRef.prefix(period),
      TriageRef.prefix(period),
      ShadowRef.prefix(period)
    )
}
