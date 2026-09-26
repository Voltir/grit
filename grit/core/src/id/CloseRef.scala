package grit.core.id

/** One attempt to close `period`, made when `last` was its newest turn. */
final case class CloseRef(period: PeriodRef, last: TurnSeq) {

  /** `close:{conversationId}:{period}:{last}`: enqueued twice, it runs once. An attempt
    * abandoned because a turn came in is retried under that turn's id.
    */
  def workflowId: WorkflowId =
    WorkflowId(
      s"${CloseRef.Prefix}:${ConversationId.value(period.conversationId)}:" +
        s"${PeriodSeq.value(period.seq)}:${TurnSeq.value(last)}"
    )
}

object CloseRef {

  private val Prefix = "close"

  /** The attempt whose workflow id is `id`, or `None` if `id` is not a close's. */
  def fromWorkflowId(id: WorkflowId): Option[CloseRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Prefix, conversation, period, last) if conversation.nonEmpty =>
        for {
          p <- period.toLongOption.flatMap(PeriodSeq.of)
          t <- last.toLongOption.filter(_ >= 0)
        } yield CloseRef(PeriodRef(ConversationId(conversation), p), TurnSeq(t))
      case _ => None
    }
}
