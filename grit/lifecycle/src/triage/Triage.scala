package grit.lifecycle.triage

import grit.core.durable.Durable
import grit.core.id.{EntryId, TriageRef, TurnSeq, WorkflowId}
import grit.core.period.Probability
import grit.core.store.{Entry, Payload, Speakers, StoreError}
import grit.core.triage.{Kind, Tags}
import grit.lifecycle.transcript.PeriodTranscript

/** The triage: one workflow per heard message ([[TriageRef]]), run on the turns' queue under
  * its conversation, so ahead of any later close of it. Each step's output is recorded, so a
  * triage resumed after a crash never asks the classifier twice. It writes no entry, so it is
  * never a period's activity and never moves its deadline.
  *
  *   1. `ask` — one classifier call over the heard message, who said it, and the thread
  *      before it ([[TriageQuestion]]); an absent or failing classifier, or an answer that
  *      does not read, is `Unanswered` tags. Nothing is asked when the message cannot be read
  *      or is gone.
  *   1. `record` — the tags kept ([[grit.core.triage.TriageStore.record]]); ignored when the
  *      message is gone or already tagged.
  */
object Triage {

  /** The triage's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Ask = "ask"
    val Record = "record"
  }

  /** The triage workflow's body, for the heard message whose workflow id is `workflowId`.
    * Returns what it did, for logs: the tags are in the store.
    */
  def body(env: TriageEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    TriageRef.fromWorkflowId(workflowId) match {
      case None => s"not a triage: ${WorkflowId.value(workflowId)}"
      case Some(triage) =>
        import TriageJournal.given
        val asked = d.step(Step.Ask)(() => ask(env, triage))
        asked match {
          case Left(why) => s"failed: $why"
          case Right((entry, tags)) =>
            val at = env.clock.now()
            d.transact(Step.Record)(
              env.records.triage.record(entry, tags, at).left.map(describe)
            ) match {
              case Left(why) => s"failed: $why"
              case Right(false) => "ignored: the message is gone or tagged already"
              case Right(true) => s"tagged: ${shown(tags)}"
            }
        }
    }

  /** The `ask` step: the heard message that is `triage`'s turn, and what the classifier made
    * of it; why not, when it cannot be read or is not there.
    */
  private def ask(env: TriageEnv^, triage: TriageRef): Either[String, (EntryId, Tags)] =
    for {
      all <- env.db
        .read(env.records.entries.list(triage.period.conversationId))
        .left
        .map(e => s"thread unread: ${describe(e)}")
      heard <- all
        .collectFirst {
          case e @ Entry(_, _, turn, _, _, Payload.Heard(_), _) if turn == triage.turn => e
        }
        .toRight(s"no heard message at turn ${TurnSeq.value(triage.turn)}")
    } yield {
      val before = all.filter(_.seq < heard.seq)
      // Unread names are no names: each line is then its role's.
      val names = PeriodTranscript
        .speakers(env.db, env.records.principals, before :+ heard)
        .getOrElse(Speakers.none)
      val text = heard.payload match {
        case Payload.Heard(t) => t
        case _ => ""
      }
      val state = TriageQuestion.State(
        text,
        names.of(heard.id).getOrElse("Someone"),
        PeriodTranscript.of(before, names)
      )
      (heard.id, TriageQuestion.judge(env.classifier, state))
    }

  private def shown(tags: Tags): String = tags match {
    case Tags.Weighed(kind, kindP, _, durable, _, model, _) =>
      s"${Kind.written(kind)} ${Probability.value(kindP)}, durable ${Probability.value(durable)} ($model)"
    case Tags.Unanswered(why) => s"unanswered: $why"
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
