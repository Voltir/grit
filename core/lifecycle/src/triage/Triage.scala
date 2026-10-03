package grit.lifecycle.triage

import grit.core.durable.Durable
import grit.core.id.{EntryId, TriageRef, TurnRef, WorkflowId}
import grit.core.period.Probability
import grit.core.speech.{Decision, SpeechJson}
import grit.core.stitch.{Placed, StitchJson, StitchReads, Stitching}
import grit.core.store.StoreError
import grit.core.triage.{Kind, Tags}

/** The triage: one workflow per heard message ([[TriageRef]]), run on the turns' queue under
  * its conversation, so ahead of any later close of it. Each step's output is recorded, so a
  * triage resumed after a crash never asks the classifier twice. It writes no entry, so it is
  * never a period's activity and never moves its deadline.
  *
  *   1. `stitch` — when the heard message is its conversation's first, in a stitchable
  *      origin: where it goes among its room's exchanges ([[Stitching.turn]]); nothing is
  *      asked otherwise, or when nothing is offered.
  *   1. `record-stitch` — that placement kept ([[grit.core.stitch.StitchStore.record]]),
  *      only when one was made.
  *   1. `ask` — one classifier call over the heard message, who said it, and the thread
  *      before it: its strand's, then its own ([[TriageInput.build]], [[TriageQuestion]]); an
  *      absent or failing classifier, or an answer that does not read, is `Unanswered` tags.
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
    val Stitch = "stitch"
    val RecordStitch = "record-stitch"
    val Ask = "ask"
    val Record = "record"
    val Consider = "consider"
    val Start = "start"
  }

  /** The triage workflow's body, for the heard message whose workflow id is `workflowId`.
    * Returns what it did, for logs: the tags are in the store.
    */
  def body(env: TriageEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    TriageRef.fromWorkflowId(workflowId) match {
      case None => s"not a triage: ${WorkflowId.value(workflowId)}"
      case Some(triage) =>
        import TriageJournal.given
        val stitched = d.step(Step.Stitch)(() => stitch(env, triage))
        val kept = stitched match {
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
    * of it; why not, when it cannot be read or is not there.
    */
  private def ask(env: TriageEnv^, triage: TriageRef): Either[String, (EntryId, Tags)] = {
    val r = env.records
    TriageInput
      .build(
        StitchReads(r.entries, r.conversations, r.lifecycle, r.stitches, r.search, r.principals),
        r.rooms,
        env.db,
        triage,
        env.tuning,
        TriageRecipe.Shipped
      )
      .map((heard, state) =>
        (heard, TriageQuestion.judge(env.classifier, TriageQuestion.Wording.Shipped, state))
      )
  }

  private def shown(tags: Tags): String = tags match {
    case Tags.Weighed(kind, kindP, _, durable, _, model, _) =>
      s"${Kind.written(kind)} ${Probability.value(kindP)}, durable ${Probability.value(durable)} ($model)"
    case Tags.Unanswered(why) => s"unanswered: $why"
  }
}
