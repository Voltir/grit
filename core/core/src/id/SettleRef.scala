package grit.core.id

import java.time.Instant
import java.time.temporal.ChronoUnit

/** The one time the classifier is asked whether `period` is finished while it is quiet
  * after its newest activity at `activity` (to the millisecond), `last` its newest turn. New
  * activity makes a new question.
  */
final case class SettleRef private (period: PeriodRef, last: TurnSeq, activity: Instant) {

  /** `settle:{conversationId}:{period}:{last}:{activity, in epoch milliseconds}`: enqueued
    * twice, it runs once, and a question is never asked twice.
    */
  def workflowId: WorkflowId =
    WorkflowId(s"${SettleRef.prefix(period)}${TurnSeq.value(last)}:${activity.toEpochMilli}")
}

object SettleRef {

  private val Name = "settle"

  /** The question about `period` quiet since `activity`, cut to the millisecond, at its turn
    * `last`.
    */
  def apply(period: PeriodRef, last: TurnSeq, activity: Instant): SettleRef =
    new SettleRef(period, last, activity.truncatedTo(ChronoUnit.MILLIS))

  /** What the workflow id of every question about `period` starts with, and no other
    * period's.
    */
  def prefix(period: PeriodRef): String =
    s"$Name:${ConversationId.value(period.conversationId)}:${PeriodSeq.value(period.seq)}:"

  /** The question whose workflow id is `id`, or `None` if `id` is not a settle's. */
  def fromWorkflowId(id: WorkflowId): Option[SettleRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Name, conversation, period, last, activity) if conversation.nonEmpty =>
        for {
          p <- period.toLongOption.flatMap(PeriodSeq.of)
          t <- last.toLongOption.filter(_ >= 0)
          a <- activity.toLongOption
        } yield SettleRef(
          PeriodRef(ConversationId(conversation), p),
          TurnSeq(t),
          Instant.ofEpochMilli(a)
        )
      case _ => None
    }
}
