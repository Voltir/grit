package grit.core.period

import grit.core.id.{CloseRef, PeriodRef, TurnRef, TurnSeq, WorkflowId}

/** A closed period whose raw entries are still kept, its turns running `first` to `last`. */
final case class Purgeable(period: PeriodRef, first: TurnSeq, last: TurnSeq) {

  /** Every workflow its purge deletes: each of its turns', and each close attempt that
    * could have been made on it, which is named by one of its turns.
    */
  def workflows: Vector[WorkflowId] = {
    val turns = (TurnSeq.value(first) to TurnSeq.value(last)).toVector.map(TurnSeq(_))
    turns.map(TurnRef(period.conversationId, _).workflowId) ++
      turns.map(CloseRef(period, _).workflowId)
  }
}
