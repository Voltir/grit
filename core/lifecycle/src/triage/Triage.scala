package grit.lifecycle.triage

import grit.core.classify.{Answer, ClassifierError}
import grit.core.durable.Durable
import grit.core.id.{EntryId, QuestionName, TriageRef, TurnRef, WorkflowId}
import grit.core.speech.{Decision, SpeechJson}
import grit.core.stitch.{Opening, Placed, StitchJson, StitchReads, Stitching}
import grit.core.store.StoreError
import grit.core.triage.{KnowledgeSources, Tags}

/** The triage: one workflow per heard message ([[TriageRef]]), run on the turns' queue under
  * its conversation, so ahead of any later close of it. Each step's output is recorded, so a
  * triage resumed after a crash never asks the classifier twice. It writes no entry, so it is
  * never a period's activity and never moves its deadline.
  *
  *   1. `stitched` — when the heard message is an [[Opening]], its placement waited for
  *      ([[grit.core.stitch.Placements.awaited]]), so the message is asked about with its
  *      strand, and only once every opening heard before it in its room is placed; nothing is
  *      waited for otherwise. A triage that passed this point before it shipped took
  *      `stitch` ([[Stitching.turn]]) and `record-stitch` instead, placing the opening itself
  *      ([[Patches.StitchInRoomOrder]]).
  *   1. `ask` — one classifier call over the heard message, who said it, and the thread
  *      before it: its strand's, then its own ([[TriageInput.heard]]), asked the questions
  *      live triage asks ([[TriageQuestions.Shipped]]), one per knowledge source covering its
  *      conversation where the set asks so; an absent or failing classifier, or an answer
  *      that does not read, is `Unanswered` tags.
  *      Nothing is asked when the message cannot be read or is gone.
  *   1. `record` — the tags kept ([[grit.core.triage.TriageStore.record]]); ignored when the
  *      message is gone or already tagged.
  *   1. `consider` — when it was tagged now, whether grit drafts a reply to it
  *      ([[grit.core.speech.Speech.decide]], against the speech ledger), the decision kept,
  *      held or drafting, in one transaction; a deployment that does not speak keeps none.
  *   1. `start` — when drafting, the heard message's own turn started, a turn rooted on it
  *      ([[grit.core.store.Payload.Heard]]).
  */
object Triage {
  import TriageInput.describe

  /** The triage's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Stitched = "stitched"
    val Stitch = "stitch"
    val RecordStitch = "record-stitch"
    val Ask = "ask"
    val Record = "record"
    val Consider = "consider"
    val Start = "start"
  }

  /** The patches the triage's steps have taken within this epoch (ADR 0004). */
  object Patches {

    /** A heard opening is placed by a workflow of its own, in its room's order, which the
      * triage waits for in `stitched`, not by the triage in `stitch` and `record-stitch`
      * (2026-10-03).
      */
    val StitchInRoomOrder = "stitch-in-room-order"
  }

  /** The triage workflow's body, for the heard message whose workflow id is `workflowId`.
    * Returns what it did, for logs: the tags are in the store.
    */
  def body(env: TriageEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    TriageRef.fromWorkflowId(workflowId) match {
      case None => s"not a triage: ${WorkflowId.value(workflowId)}"
      case Some(triage) =>
        import TriageJournal.given
        val kept =
          if (d.patch(Patches.StitchInRoomOrder))
            d.step(Step.Stitched)(() => placed(env, triage)) match {
              case Right(Some(what)) => s"placed: $what; "
              case Right(None) => ""
              case Left(why) => s"not placed: $why; "
            }
          else
            d.step(Step.Stitch)(() => stitch(env, triage)) match {
              case Right(Some((root, placed))) =>
                val at = env.clock.now()
                val stitches = env.records.stitches
                d.transact(Step.RecordStitch)(
                  stitches.record(root, placed, at).left.map(describe)
                ) match {
                  case Right(_) => s"stitched: ${StitchJson.kindOf(placed)}; "
                  case Left(why) => s"stitch not kept: $why; "
                }
              case Right(None) => ""
              case Left(why) => s"not stitched: $why; "
            }
        val asked = d.step(Step.Ask)(() => ask(env, triage))
        kept + (asked match {
          case Left(why) => s"failed: $why"
          case Right((entry, tags)) =>
            val at = env.clock.now()
            d.transact(Step.Record)(
              env.records.triage.record(entry, tags, at).left.map(describe)
            ) match {
              case Left(why) => s"failed: $why"
              case Right(false) => "ignored: the message is gone or tagged already"
              case Right(true) =>
                val now = env.clock.now()
                val (records, speech) = (env.records, env.speech)
                val considered = d.transact(Step.Consider)(
                  Speak.consider(records, speech.speaking, speech.budget, triage, entry, tags, now)
                )
                s"tagged: ${shown(tags)}; " + (considered match {
                  case Left(why) => s"not considered: $why"
                  case Right(Decision.Held(why)) =>
                    s"held: ${SpeechJson.writeSilence(why).render()}"
                  case Right(Decision.Drafting(turn)) =>
                    val start = speech.start
                    d.step(Step.Start)(() => start(turn).map(_ => turn.workflowId)) match {
                      case Left(why) => s"drafting, not started: $why"
                      case Right(queued) => s"drafting: ${WorkflowId.value(queued)}"
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
      .read {
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

  /** The `stitch` step: where the heard message that is `triage`'s turn goes among its
    * room's exchanges, when it is its conversation's first message ([[Stitching.turn]]);
    * `None` when it is not, is gone, or nothing was asked. Why not, when the store cannot be
    * read.
    */
  private def stitch(
      env: TriageEnv^,
      triage: TriageRef
  ): Either[String, Option[(EntryId, Placed)]] = {
    val r = env.records
    Stitching
      .turn(
        env.classifier,
        StitchReads(r.entries, r.conversations, r.lifecycle, r.stitches, r.search, r.principals),
        env.db,
        TurnRef(triage.period.conversationId, triage.turn),
        env.tuning
      )
      .left
      .map(e => s"room unread: ${describe(e)}")
  }

  /** The `ask` step: the heard message that is `triage`'s turn, and what the classifier made
    * of it, asked with the knowledge sources covering its conversation; why not, when it
    * cannot be read or is not there.
    */
  private def ask(env: TriageEnv^, triage: TriageRef): Either[String, (EntryId, Tags)] = {
    val r = env.records
    TriageInput
      .heard(
        StitchReads(r.entries, r.conversations, r.lifecycle, r.stitches, r.search, r.principals),
        r.rooms,
        env.db,
        TurnRef(triage.period.conversationId, triage.turn),
        env.tuning,
        TriageRecipe.Shipped
      )
      .map { read =>
        // A conversation not found is at no place, so no source covers it.
        val sources = read.place.fold(KnowledgeSources.Empty)(env.sources.at)
        (
          read.entry,
          TriageQuestions.Shipped.ask(env.classifier, read.state, sources) match {
            case Right(a) => Tags.Weighed(a.value, a.model, a.usage)
            case Left(ClassifierError.Unavailable(why)) => Tags.Unanswered(s"unavailable: $why")
            case Left(ClassifierError.Unreadable(why)) => Tags.Unanswered(s"unreadable: $why")
          }
        )
      }
  }

  /** Each answer as its name and what it read: a yes/no's probability of yes, a choice's
    * choice and its weight.
    */
  private def shown(tags: Tags): String = tags match {
    case Tags.Weighed(answers, model, _) =>
      answers.toVector
        .map { (name, answer) =>
          val read = answer match {
            case Answer.YesNo(yes) => s"$yes"
            case Answer.Choice(choice, weights, _) =>
              s"$choice ${weights.find(_.key == choice).fold(0.0)(_.probability)}"
          }
          s"${QuestionName.value(name)} $read"
        }
        .mkString("", ", ", s" ($model)")
    case Tags.Unanswered(why) => s"unanswered: $why"
  }
}
