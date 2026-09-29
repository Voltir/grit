package grit.core.id

/** The one triage of the heard message that is turn `turn` of `period`
  * ([[grit.core.store.Payload.Heard]]).
  */
final case class TriageRef(period: PeriodRef, turn: TurnSeq) {

  /** `triage:{conversationId}:{period}:{turn}`: enqueued twice, it runs once. */
  def workflowId: WorkflowId = WorkflowId(s"${TriageRef.prefix(period)}${TurnSeq.value(turn)}")
}

object TriageRef {

  private val Name = "triage"

  /** What the workflow id of every triage in `period` starts with, and no other period's. */
  def prefix(period: PeriodRef): String =
    s"$Name:${ConversationId.value(period.conversationId)}:${PeriodSeq.value(period.seq)}:"

  /** The triage whose workflow id is `id`, or `None` if `id` is not a triage's. */
  def fromWorkflowId(id: WorkflowId): Option[TriageRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Name, conversation, period, turn) if conversation.nonEmpty =>
        for {
          p <- period.toLongOption.flatMap(PeriodSeq.of)
          t <- turn.toLongOption.filter(_ >= 0)
        } yield TriageRef(PeriodRef(ConversationId(conversation), p), TurnSeq(t))
      case _ => None
    }
}
