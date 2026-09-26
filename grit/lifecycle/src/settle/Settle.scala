package grit.lifecycle.settle

import grit.core.durable.Durable
import grit.core.id.{EntryId, SettleRef, TurnSeq, WorkflowId}
import grit.core.period.{Judgement, Probability, Verdict}
import grit.core.store.{StoreError, Tx}
import grit.lifecycle.transcript.PeriodTranscript

/** The settle: one workflow per question whether anyone is waiting on a quiet period
  * ([[SettleRef]]), run on the turns' queue under its conversation, so never beside one of
  * its turns. Each step's output is recorded, so a settle resumed after a crash never asks
  * the classifier twice, and a question is asked once: its id names the quiet stretch.
  *
  *   1. `check` — the period as it stands: abandoned, asking nothing, when it is not open,
  *      has had activity since the question was made (the sweep makes a new one), or is not
  *      to be asked under the settings in force ([[grit.core.period.Deadline.ask]]).
  *   1. `ask` — one classifier call over the period's transcript ([[SettleQuestion]]); an
  *      absent or failing classifier, or an answer that does not read, is a judgement too.
  *   1. `record` — the verdict kept ([[grit.core.store.PeriodStore.judged]]); ignored when
  *      a turn came in meanwhile.
  */
object Settle {

  /** The settle's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Check = "check"
    val Ask = "ask"
    val Record = "record"
  }

  /** What `check` found. */
  enum Checked {

    /** The period is to be asked about; its turns start at `first`. */
    case Asking(first: TurnSeq)

    /** The question is over without asking, for `why`. */
    case Abandoned(why: String)
  }

  /** The settle workflow's body, for the question whose workflow id is `workflowId`.
    * Returns what it did, for logs: the verdict is in the store.
    */
  def body(env: SettleEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    SettleRef.fromWorkflowId(workflowId) match {
      case None => s"not a settle: ${WorkflowId.value(workflowId)}"
      case Some(question) =>
        import SettleJournal.given
        val records = env.records
        d.transact(Step.Check)(check(records, question)) match {
          case Left(why) => s"failed: $why"
          case Right(Checked.Abandoned(why)) => s"abandoned: $why"
          case Right(Checked.Asking(first)) =>
            val judgement = d.step(Step.Ask) { () =>
              val entries = PeriodTranscript.entries(
                env.db,
                env.records.entries,
                question.period,
                first,
                question.last
              )
              entries match {
                case Left(error) => Judgement.Unanswered(s"transcript unread: ${describe(error)}")
                case Right(own) =>
                  SettleQuestion.judge(
                    env.classifier,
                    SettleQuestion.Transcript(PeriodTranscript.of(own))
                  )
              }
            }
            val verdict = Verdict(env.clock.now(), question.last, judgement)
            d.transact(Step.Record)(
              records.periods.judged(question.period, verdict).left.map(describe)
            ) match {
              case Left(why) => s"failed: $why"
              case Right(false) => "ignored: a turn came in while it was asked"
              case Right(true) => s"judged: ${shown(judgement)}"
            }
        }
    }

  /** The `check` step. */
  private def check(records: SettleRecords, question: SettleRef)(using
      Tx^
  ): Either[String, Checked] =
    (for {
      period <- records.periods.get(question.period)
      activity <- records.periods.activity(question.period)
      settings <- records.lifecycle.current()
    } yield (period, activity) match {
      case (Some(p), Some(a)) =>
        if (a.last != question.last) Checked.Abandoned(s"turn ${TurnSeq.value(a.last)} came in")
        else if (a.question != question) Checked.Abandoned(s"active since, at ${a.newest}")
        else if (a.asks(settings).isEmpty)
          Checked.Abandoned("not to be asked: judged already, out of asks, or asking is off")
        else Checked.Asking(p.first)
      case (Some(_), None) => Checked.Abandoned("closed")
      case (None, _) => Checked.Abandoned("no such period")
    }).left.map(describe)

  private def shown(judgement: Judgement): String = judgement match {
    case Judgement.Weighed(nobody, _, _, model) =>
      s"nobody ${Probability.value(nobody)} ($model)"
    case Judgement.Unanswered(why) => s"unanswered: $why"
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
