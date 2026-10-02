package grit.core.id

/** One shadow variant's run on one heard message: the `triage` it shadows and the variant's
  * `name`.
  */
final case class ShadowRef(triage: TriageRef, name: ShadowName) {

  /** `shadow:{conversationId}:{period}:{turn}:{name}`: enqueued twice, it runs once. */
  def workflowId: WorkflowId =
    WorkflowId(
      s"${ShadowRef.prefix(triage.period)}${TurnSeq.value(triage.turn)}:${ShadowName.value(name)}"
    )
}

object ShadowRef {

  private val Name = "shadow"

  /** What the workflow id of every shadow of a message heard in `period` starts with, and no
    * other period's.
    */
  def prefix(period: PeriodRef): String =
    s"$Name:${ConversationId.value(period.conversationId)}:${PeriodSeq.value(period.seq)}:"

  /** The shadow whose workflow id is `id`, or `None` if `id` is not a shadow's. */
  def fromWorkflowId(id: WorkflowId): Option[ShadowRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Name, conversation, period, turn, name) if conversation.nonEmpty =>
        for {
          p <- period.toLongOption.flatMap(PeriodSeq.of)
          t <- turn.toLongOption.filter(_ >= 0)
          n <- ShadowName.of(name).toOption
        } yield ShadowRef(TriageRef(PeriodRef(ConversationId(conversation), p), TurnSeq(t)), n)
      case _ => None
    }
}
