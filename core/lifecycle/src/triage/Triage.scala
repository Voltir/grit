package grit.lifecycle.triage

import grit.core.classify.{Answer, ClassifierError}
import grit.core.durable.Durable
import grit.core.id.{EntryId, QuestionName, TriageRef, TurnRef, WorkflowId}
import grit.core.route.Routed
import grit.core.speech.{Decision, Outcome, SpeechJson}
import grit.core.stitch.{Opening, StitchReads}
import grit.core.store.StoreError
import grit.core.triage.{Corpora, Tags}
import grit.core.visibility.Subject

/** The triage: one workflow per heard message ([[TriageRef]]), run on the turns' queue under
  * its conversation, so ahead of any later close of it. Each step's output is recorded, so a
  * triage resumed after a crash never asks the classifier twice. It writes no entry, so it is
  * never a period's activity and never moves its deadline.
  *
  *   1. `stitched` — when the heard message is an [[Opening]], its placement waited for
  *      ([[grit.core.stitch.Placements.awaited]]), so the message is asked about with its
  *      strand, and only once every opening heard before it in its room is placed; nothing is
  *      waited for otherwise.
  *   1. `ask` — one classifier call over the heard message, who said it, and the thread
  *      before it: its strand's, then its own ([[TriageInput.heard]]), asked the questions
  *      live triage asks (`env.questions`, [[TriageQuestions.shipped]] in a deployment), one per corpus covering its
  *      conversation where the set asks so; an absent or failing classifier, or an answer
  *      that does not read, is `Unanswered` tags.
  *      Nothing is asked when the message cannot be read or is gone.
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

  /** The triage's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Stitched = "stitched"
    val Ask = "ask"
    val Record = "record"
    val Consider = "consider"
    val Start = "start"

    /** Keeps a drafting turn whose start failed settled as failed, so it holds no later
      * message in its thread.
      */
    val Unstarted = "unstarted"
  }

  /** The triage workflow's body, for the heard message whose workflow id is `workflowId`.
    * Returns what it did, for logs: the tags are in the store. A drafting turn whose start
    * fails is kept settled as [[grit.core.speech.Outcome.Failed]], with why; one answering as
    * said to grit stays awaited, and its edge starts it again when it next starts.
    */
  def body(env: TriageEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    TriageRef.fromWorkflowId(workflowId) match {
      case None => s"not a triage: ${WorkflowId.value(workflowId)}"
      case Some(triage) =>
        import TriageJournal.given
        val subject = Subject.Conversation(triage.period.conversationId)
        val kept = d.step(Step.Stitched)(() => placed(env, triage)) match {
          case Right(Some(what)) => s"placed: $what; "
          case Right(None) => ""
          case Left(why) => s"not placed: $why; "
        }
        val asked = d.step(Step.Ask)(() => ask(env, triage))
        kept + (asked match {
          case Left(why) => s"failed: $why"
          case Right((entry, tags)) =>
            val at = env.clock.now()
            d.transact(Step.Record, subject)(
              env.records.triage.record(entry, tags, at).left.map(describe)
            ) match {
              case Left(why) => s"failed: $why"
              case Right(false) => "ignored: the message is gone or tagged already"
              case Right(true) =>
                val now = env.clock.now()
                val (records, speech) = (env.records, env.speech)
                def started(turn: TurnRef): Either[String, WorkflowId] = {
                  val start = speech.start
                  d.step(Step.Start)(() => start(turn).map(_ => turn.workflowId))
                }
                val considered = d.transact(Step.Consider, subject)(
                  Speak.consider(records, speech.speaking, speech.budget, triage, entry, tags, now)
                )
                s"tagged: ${shown(tags)}; " + (considered match {
                  case Left(why) => s"not considered: $why"
                  case Right(Decision.Held(why)) =>
                    s"held: ${SpeechJson.writeSilence(why).render()}"
                  case Right(Decision.Drafting(turn)) =>
                    started(turn) match {
                      case Right(queued) => s"drafting: ${WorkflowId.value(queued)}"
                      case Left(why) =>
                        // A drafting row left unsettled holds every later unprompted message
                        // in its thread unanswered, so it is settled failed. An enqueue that
                        // committed yet reported a failure still runs its turn, which posts
                        // while the ledger says settled: the hold is released either way.
                        val failed = Outcome.Failed(s"not started: $why")
                        val store = records.speech
                        d.transact(Step.Unstarted, subject)(
                          store.drafted(turn, failed, None, now).left.map(describe)
                        ) match {
                          case Left(e) => s"drafting, not started: $why; not settled: $e"
                          case Right(_) => s"drafting, not started: $why"
                        }
                    }
                  // Left unsettled: its reply was awaited in `consider`'s transaction, so its
                  // edge starts it when it next starts, and an answering row counts in no rate
                  // and holds no later message.
                  case Right(Decision.Answering(turn, _)) =>
                    started(turn) match {
                      case Right(queued) => s"answering: ${WorkflowId.value(queued)}"
                      case Left(why) => s"answering, not started: $why"
                    }
                })
            }
        })
    }

  /** The `stitched` step: what the placement of the heard message that is `triage`'s turn
    * did, once it has ended, when the message is an [[Opening]]; `None` when it is not, or is
    * gone. Why not, when the store cannot be read or the placement failed.
    */
  private def placed(env: TriageEnv^, triage: TriageRef): Either[String, Option[String]] = {
    val r = env.records
    val conversation = triage.period.conversationId
    env.db
      .read(Subject.Conversation(conversation)) {
        for {
          all <- r.entries.list(conversation)
          c <- r.conversations.get(conversation)
        } yield c.flatMap(Opening.of(_, all, TurnRef(conversation, triage.turn)))
      }
      .left
      .map(e => s"thread unread: ${describe(e)}")
      .flatMap {
        case None => Right(None)
        case Some(opening) => env.placements.awaited(opening).map(Some(_))
      }
  }

  /** The `ask` step: the heard message that is `triage`'s turn, and what the classifier made
    * of it, asked with the corpora covering its conversation; why not, when it
    * cannot be read or is not there.
    */
  private def ask(env: TriageEnv^, triage: TriageRef): Either[String, (EntryId, Tags)] = {
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
          .map { read =>
            // A conversation not found is at no place, so no source covers it.
            val sources = read.place.fold(Corpora.Empty)(env.sources.at)
            (
              read.entry,
              env.questions.ask(env.classifier.asking(routed), read.state, sources) match {
                case Right(a) => Tags.Weighed(a.value, a.model, a.usage)
                case Left(ClassifierError.Unavailable(why)) =>
                  Tags.Unanswered(s"unavailable: $why")
                case Left(ClassifierError.Unreadable(why)) => Tags.Unanswered(s"unreadable: $why")
              }
            )
          }
      }
      .left
      .map(e => s"thread unread: ${describe(e)}")
      .flatten
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
