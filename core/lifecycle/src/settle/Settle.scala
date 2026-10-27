package grit.lifecycle.settle

import grit.act.moves.Judging
import grit.core.act.MoveName
import grit.core.durable.{Durable, Flow, Flows, Step, StepName}
import grit.core.id.{EntryId, SettleRef, TurnRef, TurnSeq, WorkflowId}
import grit.core.period.{Judgement, Probability, Verdict}
import grit.core.route.Routed
import grit.core.store.{Speakers, StoreError, Tx}
import grit.core.visibility.Subject
import grit.lifecycle.transcript.PeriodTranscript

/** The settle: one workflow per question whether anyone is waiting on a quiet period
  * ([[SettleRef]]), run on the turns' queue under its conversation, so never beside one of
  * its turns. Each step's output is recorded, so a settle resumed after a crash never asks
  * the classifier twice, and a question is asked once: its id names the quiet stretch.
  *
  *   1. `check` — the period as it stands: abandoned, asking nothing, when it is not open,
  *      has had activity since the question was made (the sweep makes a new one), or is not
  *      to be asked under the settings in force ([[grit.core.period.Deadline.ask]]).
  *   1. `ask:settle` — one classifier call over the period's transcript ([[SettleQuestion]]),
  *      a move ([[grit.act.moves.Judging]]): admitted against the day's cap, so one over it is
  *      unanswered; an absent or failing classifier, or an answer that does not read, is a
  *      judgement too. `ask:settle:record` — when the classifier answered, its cost in the
  *      ledger, charged to the question's last turn; a store that fails it fails the settle.
  *   1. `record` — the verdict kept ([[grit.core.store.PeriodStore.judged]]); ignored when
  *      a turn came in meanwhile.
  */
object Settle {

  /** What `check` found. */
  enum Checked {

    /** The period is to be asked about; its turns start at `first`. */
    case Asking(first: TurnSeq)

    /** The question is over without asking, for `why`. */
    case Abandoned(why: String)
  }

  import SettleJournal.given

  /** The question `ref`, to be asked over its period's turns from `first`. */
  private final case class Asking(ref: SettleRef, first: TurnSeq)

  /** The question `ref`, and its answer. */
  private final case class Judged(ref: SettleRef, judgement: Judgement)

  private def subject(ref: SettleRef): Subject = Subject.Conversation(ref.period.conversationId)

  private val checked: Step[SettleEnv, SettleRef, Either[String, Checked]] =
    Step.transact[SettleEnv, SettleRef, Either[String, Checked]](StepName("check"), subject)(env =>
      ref => check(env.records, ref)
    )

  private val asked: Flow[SettleEnv, Asking, Nothing, Judging.Judged[Judgement]] =
    Judging.flow[SettleEnv, Asking, Judgement](
      MoveName("settle"),
      a =>
        Judging.Charged(
          TurnRef(a.ref.period.conversationId, a.ref.last),
          a.ref.workflowId,
          subject(a.ref)
        )
    )(_.moves, _.budget)(env => a => judge(env, a))

  private val recorded: Step[SettleEnv, Judged, Either[String, Boolean]] =
    Step.transact[SettleEnv, Judged, Either[String, Boolean]](
      StepName("record"),
      j => subject(j.ref)
    )(env =>
      j => {
        val verdict = Verdict(env.clock.now(), j.ref.last, j.judgement)
        env.records.periods.judged(j.ref.period, verdict).left.map(describe)
      }
    )

  /** The settle workflow, from the question's workflow id: what it did, for logs, at whichever
    * end; the verdict is in the store.
    */
  val flow: Flow[SettleEnv, WorkflowId, String, String] =
    Flow
      .fromEither(WorkflowId.as("settle", SettleRef.fromWorkflowId))
      .andThen(Flow.decide(checked) {
        case (_, Left(why)) => Left(s"failed: $why")
        case (_, Right(Checked.Abandoned(why))) => Left(s"abandoned: $why")
        case (ref, Right(Checked.Asking(first))) => Right(Asking(ref, first))
      })
      .andThen(asked.decided {
        case (_, Judging.Judged(_, Left(why))) => Left(s"failed: its cost unrecorded: $why")
        case (a, Judging.Judged(judgement, Right(()))) => Right(Judged(a.ref, judgement))
      })
      .andThen(Flow.step(recorded).keeping)
      .map {
        case (_, Left(why)) => s"failed: $why"
        case (_, Right(false)) => "ignored: a turn came in while it was asked"
        case (j, Right(true)) => s"judged: ${shown(j.judgement)}"
      }

  /** The settle workflow for the question whose workflow id is `workflowId`: [[flow]] run. */
  def body(env: SettleEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    Flows.run(flow, env, workflowId).merge

  /** The `ask:settle` step: one classifier call over the transcript of `question`'s period from
    * `first`, asked only when its service is cleared for what was read.
    */
  private def judge(env: SettleEnv^, at: Asking): Judging.Pose[Judgement]^{env} = {
    val (question, first) = (at.ref, at.first)
    Routed
      .reading(env.db.as(subject(question))) { (routed, bounded) =>
        val entries = PeriodTranscript.entries(
          bounded,
          env.records.entries,
          question.period,
          first,
          question.last
        )
        entries match {
          case Left(error) =>
            Judging.Pose.Made(Judgement.Unanswered(s"transcript unread: ${describe(error)}"))
          case Right(own) =>
            // Unread names are no names: each line is then its role's.
            val names = PeriodTranscript
              .speakers(bounded, env.records.principals, own)
              .getOrElse(Speakers.none)
            SettleQuestion.judge(
              env.moves.classifier,
              routed,
              SettleQuestion.Transcript(PeriodTranscript.of(own, names))
            )
        }
      }
      .fold(
        error => Judging.Pose.Made(Judgement.Unanswered(s"transcript unread: ${describe(error)}")),
        identity
      )
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
    case StoreError.Full(bound) =>
      s"its plugin already holds the $bound documents it may, so nothing was kept"
  }
}
