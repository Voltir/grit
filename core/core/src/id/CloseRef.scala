package grit.core.id

import java.time.Instant
import java.time.temporal.ChronoUnit

/** One attempt to close `period` on the deadline `due` (to the millisecond), made when `last`
  * was its newest turn. Anything that moves the deadline, such as a turn's entries or a
  * change of settings, makes a different attempt.
  */
final case class CloseRef private (period: PeriodRef, last: TurnSeq, due: Instant) {

  /** `close:{conversationId}:{period}:{last}:{due, in epoch milliseconds}`: enqueued twice, it
    * runs once, and an attempt is never made twice.
    */
  def workflowId: WorkflowId =
    WorkflowId(s"${CloseRef.prefix(period)}${TurnSeq.value(last)}:${due.toEpochMilli}")
}

object CloseRef {

  private val Name = "close"

  /** The attempt on `period`'s deadline `due`, cut to the millisecond, made at its turn `last`. */
  def apply(period: PeriodRef, last: TurnSeq, due: Instant): CloseRef =
    new CloseRef(period, last, due.truncatedTo(ChronoUnit.MILLIS))

  /** What the workflow id of every attempt on `period` starts with, and no other period's. */
  def prefix(period: PeriodRef): String =
    s"$Name:${ConversationId.value(period.conversationId)}:${PeriodSeq.value(period.seq)}:"

  /** The attempt whose workflow id is `id`, or `None` if `id` is not a close's. */
  def fromWorkflowId(id: WorkflowId): Option[CloseRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Name, conversation, period, last, due) if conversation.nonEmpty =>
        for {
          p <- period.toLongOption.flatMap(PeriodSeq.of)
          t <- last.toLongOption.filter(_ >= 0)
          d <- due.toLongOption
        } yield CloseRef(
          PeriodRef(ConversationId(conversation), p),
          TurnSeq(t),
          Instant.ofEpochMilli(d)
        )
      case _ => None
    }
}
