package grit.lifecycle.stitch

import grit.act.moves.Judging
import grit.core.act.MoveName
import grit.core.durable.{Durable, Flow, Flows, Step, StepName}
import grit.core.id.{EntryId, StitchRef, WorkflowId}
import grit.core.route.Routed
import grit.core.stitch.{Placed, StitchJson, Stitching}
import grit.core.store.StoreError
import grit.core.visibility.Subject

/** The placement of a stitchable conversation's opening ([[grit.core.stitch.Opening]]): one
  * workflow per opening ([[StitchRef]]), run on a queue of its own one at a time per room, so
  * every opening is placed after those queued before it in its room (ADR 0023). Each step's
  * output is recorded, so a placement resumed after a crash never asks the classifier twice.
  *
  *   1. `ask:stitch` — where the opening goes among its room's exchanges
  *      ([[Stitching.placing]]), a move ([[grit.act.moves.Judging]]): admitted against the day's
  *      cap, so one over it is unread; nothing is asked when it is not an opening, is gone, is
  *      placed already, or nothing is offered. `ask:stitch:record` — when the classifier
  *      replied, its cost in the ledger, charged to the opening's turn; a store that fails it
  *      fails the placement, kept nowhere.
  *   1. `record-stitch` — that placement kept ([[grit.core.stitch.StitchStore.record]]),
  *      only when one was made.
  */
object Stitch {
  import StitchJournal.given

  /** A placement made for the opening of `ref`, whose conversation's first entry is `root`. */
  private final case class Made(ref: StitchRef, root: EntryId, placed: Placed)

  private def subject(ref: StitchRef): Subject = Subject.Conversation(ref.turn.conversationId)

  private val stitch: Flow[StitchEnv, StitchRef, Nothing, Judging.Judged[
    Either[String, Option[(EntryId, Placed)]]
  ]] =
    Judging.flow[StitchEnv, StitchRef, Either[String, Option[(EntryId, Placed)]]](
      MoveName("stitch"),
      ref => Judging.Charged(ref.turn, ref.workflowId, subject(ref))
    )(_.moves, _.budget)(env => ref => placing(env, ref))

  /** The `ask:stitch` step: where `ref`'s opening goes among its room's exchanges, the
    * classifier asked only when its service is cleared for what was read; `None` when nothing
    * is asked, why not when the room cannot be read.
    */
  private def placing(
      env: StitchEnv^,
      ref: StitchRef
  ): Judging.Pose[Either[String, Option[(EntryId, Placed)]]]^{env} = {
    def unread(e: StoreError) = Judging.Pose.Made(Left(s"room unread: ${describe(e)}"))
    Routed
      .reading(env.db.as(subject(ref))) { (routed, bounded) =>
        Stitching.placing(env.reads, bounded, ref.turn, env.tuning)(
          (first, placed) => Judging.Pose.Made(Right(Some(first -> placed))),
          (first, q) =>
            Judging.Pose.asking(env.moves.classifier, routed, q.state, q.choice)((a, _) =>
              Right(Some(first -> q.placed(a)))
            )
        ) match {
          case Left(e) => unread(e)
          case Right(None) => Judging.Pose.Made(Right(None))
          case Right(Some(pose)) => pose
        }
      }
      .fold(unread, identity)
  }

  private val record: Step[StitchEnv, Made, Either[String, Boolean]] =
    Step.transact[StitchEnv, Made, Either[String, Boolean]](
      StepName("record-stitch"),
      p => subject(p.ref)
    )(env => p => env.reads.stitches.record(p.root, p.placed, env.clock.now()).left.map(describe))

  /** The placement workflow, from the opening's workflow id: what it did, for logs, at
    * whichever end; the placement is in the store.
    */
  val flow: Flow[StitchEnv, WorkflowId, String, String] =
    Flow
      .fromEither(WorkflowId.as("stitch", StitchRef.fromWorkflowId))
      .andThen(stitch.decided {
        case (_, Judging.Judged(_, Left(why))) => Left(s"failed: its cost unrecorded: $why")
        case (ref, Judging.Judged(Right(Some((root, placed))), Right(()))) =>
          Right(Made(ref, root, placed))
        case (_, Judging.Judged(Right(None), Right(()))) => Left("nothing asked")
        case (_, Judging.Judged(Left(why), Right(()))) => Left(s"failed: $why")
      })
      .andThen(Flow.step(record).keeping)
      .map {
        case (p, Right(true)) => s"stitched: ${StitchJson.kindOf(p.placed)}"
        case (_, Right(false)) => "not kept: the opening is gone or placed already"
        case (_, Left(why)) => s"not kept: $why"
      }

  /** The placement workflow for the opening whose workflow id is `workflowId`: [[flow]] run. */
  def body(env: StitchEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    Flows.run(flow, env, workflowId).merge

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
