package grit.core.id

/** One turn of one conversation: what an edge starts, and all a turn workflow receives. */
final case class TurnRef(conversationId: ConversationId, turnSeq: TurnSeq) {

  /** The turn's workflow id, `{conversationId}:{turnSeq}`. Starting it twice runs it once. */
  def workflowId: WorkflowId =
    WorkflowId(s"${ConversationId.value(conversationId)}:${TurnSeq.value(turnSeq)}")
}

object TurnRef {

  /** The turn whose workflow id is `id`, or `None` if `id` is not a turn's. */
  def fromWorkflowId(id: WorkflowId): Option[TurnRef] =
    WorkflowId.value(id).split(':') match {
      case Array(conversation, seq) if conversation.nonEmpty =>
        seq.toLongOption.filter(_ >= 0).map(s => TurnRef(ConversationId(conversation), TurnSeq(s)))
      case _ => None
    }
}
