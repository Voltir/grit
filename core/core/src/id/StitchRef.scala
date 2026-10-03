package grit.core.id

import java.time.Instant
import java.time.temporal.ChronoUnit

/** The placement of the message that is `turn`, said `at` (to the millisecond, and no earlier
  * than 1970): one workflow per stitchable conversation's opening. Its workflow ids sort as
  * `at` does, so two queued in one millisecond run in the order said.
  */
final case class StitchRef private (turn: TurnRef, at: Instant) {

  /** `stitch:{at, epoch milliseconds in 13 digits}:{conversationId}:{turn}`: enqueued twice,
    * it runs once.
    */
  def workflowId: WorkflowId =
    WorkflowId(
      f"${StitchRef.Name}:${at.toEpochMilli}%013d:${ConversationId.value(turn.conversationId)}:${TurnSeq.value(turn.turnSeq)}"
    )
}

object StitchRef {

  private val Name = "stitch"

  /** The placement of `turn`'s message said `at`, cut to the millisecond; a time before 1970
    * is taken as 1970's start.
    */
  def apply(turn: TurnRef, at: Instant): StitchRef =
    new StitchRef(
      turn,
      (if (at.isBefore(Instant.EPOCH)) Instant.EPOCH else at).truncatedTo(ChronoUnit.MILLIS)
    )

  /** The placement whose workflow id is `id`, or `None` if `id` is not a placement's. */
  def fromWorkflowId(id: WorkflowId): Option[StitchRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Name, at, conversation, turn) if conversation.nonEmpty =>
        for {
          ms <- at.toLongOption.filter(_ >= 0)
          t <- turn.toLongOption.filter(_ >= 0)
        } yield StitchRef(
          TurnRef(ConversationId(conversation), TurnSeq(t)),
          Instant.ofEpochMilli(ms)
        )
      case _ => None
    }
}
