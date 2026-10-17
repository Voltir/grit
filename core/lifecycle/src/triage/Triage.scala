package grit.lifecycle.triage

import grit.act.moves.Judging
import grit.act.phase.Placing
import grit.core.act.MoveName
import grit.core.classify.Answer
import grit.core.durable.{Durable, Flow, Flows, Step, StepName}
import grit.core.id.{EntryId, QuestionName, TriageRef, TurnRef, WorkflowId}
import grit.core.route.Routed
import grit.core.speech.{Decision, Outcome, SpeechJson}
import grit.core.stitch.StitchReads
import grit.core.store.{Payload, StoreError}
import grit.core.triage.{Corpora, Tags}
import grit.core.visibility.Subject

/** The triage: one workflow per heard message ([[TriageRef]]), run on the turns' queue under
  * its conversation, so ahead of any later close of it. Each step's output is recorded, so a
  * triage resumed after a crash never asks the classifier twice. It writes no entry, so it is
  * never a period's activity and never moves its deadline.
  *
  *   1. `stitched` — when the heard message is an [[grit.core.stitch.Opening]], its placement
  *      waited for ([[grit.act.phase.Placing.placed]]), so the message is asked about with its
  *      strand, and only once every opening heard before it in its room is placed; nothing is
  *      waited for otherwise.
  *   1. `ask:triage` — one classifier call over the heard message, who said it, and the thread
  *      before it: its strand's, then its own ([[TriageInput.heard]]), asked the questions
  *      live triage asks (`env.questions`, [[TriageQuestions.shipped]] in a deployment), one per corpus covering its
  *      conversation where the set asks so; a move ([[grit.act.moves.Judging]]): admitted
  *      against the day's cap, so one over it is unanswered; an absent or failing classifier,
  *      or an answer that does not read, is `Unanswered` tags.
  *      Nothing is asked when the message cannot be read or is gone, and a message that says
  *      nothing ([[grit.core.store.Payload.saysNothing]]) is `Unanswered` ("nothing was said")
  *      unasked. `ask:triage:record` —
  *      when the classifier replied, its cost in the ledger, charged to the heard message's
  *      turn; a store that fails it fails the triage.
  *   1. `record` — the tags kept ([[grit.core.triage.TriageStore.record]]); ignored when the
  *      message is gone or already tagged.
  *   1. `consider` — when it was tagged now, whether grit drafts a reply to it, or answers it
  *      as said to grit ([[grit.core.speech.Speech.decide]], against the speech ledger), the
  *      decision kept, held, drafting or answering, in one transaction, with, when answering,
  *      its reply awaited and its mark wanted at its reply address; a deployment that does
  *      not speak keeps none but on a message put to grit.
  *   1. `start` — when drafting or answering, the heard message's own turn started, a turn
  *      rooted on it ([[grit.core.store.Payload.Heard]]).
  *   1. `unstarted` — when drafting and its start failed, the decision settled as failed
  *      ([[grit.core.speech.SpeechStore.drafted]]).
  */
object Triage {
  import TriageInput.describe
  import TriageJournal.given

  /** The triage `ref`, with what its placement did, for logs (`kept`, empty or ending `; `). */
  private final case class Waited(ref: TriageRef, kept: String)

  /** The triage `at`, its placement waited for, and what the classifier made of its heard message, `entry`. */
  private final case class Tagged(at: Waited, entry: EntryId, tags: Tags) {

    /** What the run did, for logs: its placement, its tags, then `what`. */
    def said(what: String): String = s"${at.kept}tagged: ${shown(tags)}; $what"
  }

  /** The tagged triage `at`, whose own turn `turn` starts, drafting or answering. */
  private final case class Starting(at: Tagged, turn: TurnRef, drafting: Boolean)

  /** The drafting turn of `at`, whose start failed for `why`. */
  private final case class Unstarted(at: Starting, why: String)

  private def subject(ref: TriageRef): Subject = Subject.Conversation(ref.period.conversationId)

  private val stitched: Flow[TriageEnv, TriageRef, Nothing, Either[String, Option[String]]] =
    Placing
      .placed[TriageRef, String](
        StepName("stitched"),
        ref => TurnRef(ref.period.conversationId, ref.turn),
        subject,
        {
          case Placing.Unplaced.Unread(e) => s"thread unread: ${describe(e)}"
          case Placing.Unplaced.Failed(why) => why
        }
      )
      .over[TriageEnv](env => env.placing)

  private val asked
      : Flow[TriageEnv, Waited, Nothing, Judging.Judged[Either[String, (EntryId, Tags)]]] =
    Judging.flow[TriageEnv, Waited, Either[String, (EntryId, Tags)]](
      MoveName("triage"),
      w =>
        Judging.Charged(
          TurnRef(w.ref.period.conversationId, w.ref.turn),
          w.ref.workflowId,
          subject(w.ref)
        )
    )(_.moves, _.budget)(env => w => ask(env, w.ref))

  private val recorded: Step[TriageEnv, Tagged, Either[String, Boolean]] =
    Step.transact[TriageEnv, Tagged, Either[String, Boolean]](
      StepName("record"),
      t => subject(t.at.ref)
    )(env => t => env.records.triage.record(t.entry, t.tags, env.clock.now()).left.map(describe))

  private val considered: Step[TriageEnv, Tagged, Either[String, Decision]] =
    Step.transact[TriageEnv, Tagged, Either[String, Decision]](
      StepName("consider"),
      t => subject(t.at.ref)
    )(env =>
      t =>
        Speak.consider(
          env.records,
          env.speech.speaking,
          env.speech.budget,
          t.at.ref,
          t.entry,
          t.tags,
          env.clock.now()
        )
    )

  private val started: Step[TriageEnv, Starting, Either[String, WorkflowId]] =
    Step[TriageEnv, Starting, Either[String, WorkflowId]](StepName("start"))(env =>
      s => env.speech.start(s.turn).map(_ => s.turn.workflowId)
    )

  /** Keeps a drafting turn whose start failed settled as failed, so it holds no later message
    * in its thread. An enqueue that committed yet reported a failure still runs its turn, which
    * posts while the ledger says settled: the hold is released either way.
    */
  private val unstarted: Step[TriageEnv, Unstarted, Either[String, Boolean]] =
    Step.transact[TriageEnv, Unstarted, Either[String, Boolean]](
      StepName("unstarted"),
      u => subject(u.at.at.at.ref)
    )(env =>
      u =>
        env.records.speech
          .drafted(u.at.turn, Outcome.Failed(s"not started: ${u.why}"), None, env.clock.now())
          .left
          .map(describe)
    )

  /** The triage workflow, from the heard message's workflow id: what it did, for logs, at
    * whichever end; the tags are in the store. A drafting turn whose start fails is kept
    * settled as [[grit.core.speech.Outcome.Failed]], with why; one answering as said to grit
    * stays awaited, and its edge starts it again when it next starts.
    */
  val flow: Flow[TriageEnv, WorkflowId, String, String] =
    Flow
      .fromEither(WorkflowId.as("triage", TriageRef.fromWorkflowId))
      .andThen(stitched.keeping.map { (ref, placement) =>
        Waited(
          ref,
          placement match {
            case Right(Some(what)) => s"placed: $what; "
            case Right(None) => ""
            case Left(why) => s"not placed: $why; "
          }
        )
      })
      .andThen(asked.decided {
        case (p, Judging.Judged(_, Left(why))) =>
          Left(s"${p.kept}failed: its cost unrecorded: $why")
        case (p, Judging.Judged(Left(why), Right(()))) => Left(s"${p.kept}failed: $why")
        case (p, Judging.Judged(Right((entry, tags)), Right(()))) => Right(Tagged(p, entry, tags))
      })
      .andThen(Flow.decide(recorded) {
        case (t, Left(why)) => Left(s"${t.at.kept}failed: $why")
        case (t, Right(false)) =>
          Left(s"${t.at.kept}ignored: the message is gone or tagged already")
        case (t, Right(true)) => Right(t)
      })
      .andThen(Flow.decide(considered) {
        case (t, Left(why)) => Left(t.said(s"not considered: $why"))
        case (t, Right(Decision.Held(why))) =>
          Left(t.said(s"held: ${SpeechJson.writeSilence(why).render()}"))
        case (t, Right(Decision.Drafting(turn))) => Right(Starting(t, turn, drafting = true))
        // Left unsettled when its start fails: its reply was awaited in `consider`'s
        // transaction, so its edge starts it when it next starts, and an answering row counts
        // in no rate and holds no later message.
        case (t, Right(Decision.Answering(turn, _))) => Right(Starting(t, turn, drafting = false))
      })
      .andThen(Flow.decide(started) {
        case (s, Right(queued)) =>
          val what = if (s.drafting) "drafting" else "answering"
          Left(s.at.said(s"$what: ${WorkflowId.value(queued)}"))
        case (s, Left(why)) if !s.drafting => Left(s.at.said(s"answering, not started: $why"))
        case (s, Left(why)) => Right(Unstarted(s, why))
      })
      .andThen(Flow.step(unstarted).keeping)
      .map {
        case (u, Left(e)) => u.at.at.said(s"drafting, not started: ${u.why}; not settled: $e")
        case (u, Right(_)) => u.at.at.said(s"drafting, not started: ${u.why}")
      }

  /** The triage workflow for the heard message whose workflow id is `workflowId`: [[flow]]
    * run.
    */
  def body(env: TriageEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    Flows.run(flow, env, workflowId).merge

  /** The `ask:triage` step: the heard message that is `triage`'s turn, and what the classifier made
    * of it, asked with the corpora covering its conversation; why not, when it
    * cannot be read or is not there.
    */
  private def ask(
      env: TriageEnv^,
      triage: TriageRef
  ): Judging.Pose[Either[String, (EntryId, Tags)]]^{env} = {
    val r = env.records
    // The classifier is asked only when its service is cleared for what the read knew.
    Routed
      .reading(env.db.as(Subject.Conversation(triage.period.conversationId))) { (routed, reads) =>
        TriageInput
          .heard(
            StitchReads(
              r.entries,
              r.conversations,
              r.lifecycle,
              r.stitches,
              r.search,
              r.principals
            ),
            r.rooms,
            reads,
            TurnRef(triage.period.conversationId, triage.turn),
            env.tuning,
            TriageRecipe.Shipped
          )
          .fold(
            why => Judging.Pose.Made(Left(why)),
            read =>
              if (Payload.saysNothing(read.state.message))
                Judging.Pose.Made(Right((read.entry, Tags.Unanswered("nothing was said"))))
              else {
                // A conversation not found is at no place, so no source covers it.
                val sources = read.place.fold(Corpora.Empty)(env.sources.at)
                val entry = read.entry
                Judging.Pose.asking(
                  env.moves.classifier,
                  routed,
                  read.state,
                  env.questions.asked(sources)
                ) {
                  case (Right(a), _) => Right((entry, Tags.Weighed(a.value, a.model, a.usage)))
                  case (Left(error), _) => Right((entry, Tags.Unanswered(error.said)))
                }
              }
          )
      }
      .fold(e => Judging.Pose.Made(Left(s"thread unread: ${describe(e)}")), identity)
  }

  /** Each answer as its name and what it read: a yes/no's probability of yes, a choice's
    * choice and its weight, a score's position.
    */
  private def shown(tags: Tags): String = tags match {
    case Tags.Weighed(answers, model, _) =>
      answers.toVector
        .map { (name, answer) =>
          val read = answer match {
            case Answer.YesNo(yes) => s"$yes"
            case Answer.Choice(choice, weights, _) =>
              s"$choice ${weights.find(_.key == choice).fold(0.0)(_.probability)}"
            case Answer.Score(score, _, _) => s"score $score"
          }
          s"${QuestionName.value(name)} $read"
        }
        .mkString("", ", ", s" ($model)")
    case Tags.Unanswered(why) => s"unanswered: $why"
  }
}
