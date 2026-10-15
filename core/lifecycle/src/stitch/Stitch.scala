package grit.lifecycle.stitch

import grit.core.durable.{Durable, Flow, Flows, Step, StepName}
import grit.core.id.{EntryId, StitchRef, WorkflowId}
import grit.core.stitch.{Placed, StitchJson, Stitching}
import grit.core.store.StoreError
import grit.core.visibility.Subject

/** The placement of a stitchable conversation's opening ([[grit.core.stitch.Opening]]): one
  * workflow per opening ([[StitchRef]]), run on a queue of its own one at a time per room, so
  * every opening is placed after those queued before it in its room (ADR 0023). Each step's
  * output is recorded, so a placement resumed after a crash never asks the classifier twice.
  *
  *   1. `stitch` — where the opening goes among its room's exchanges ([[Stitching.turn]]);
  *      nothing is asked when it is not an opening, is gone, is placed already, or nothing is
  *      offered.
  *   1. `record-stitch` — that placement kept ([[grit.core.stitch.StitchStore.record]]),
  *      only when one was made.
  */
object Stitch {
  import StitchJournal.given

  /** A placement made for the opening of `ref`, whose conversation's first entry is `root`. */
  private final case class Made(ref: StitchRef, root: EntryId, placed: Placed)

  private def subject(ref: StitchRef): Subject = Subject.Conversation(ref.turn.conversationId)

  private val stitch: Step[StitchEnv, StitchRef, Either[String, Option[(EntryId, Placed)]]] =
    Step[StitchEnv, StitchRef, Either[String, Option[(EntryId, Placed)]]](StepName("stitch")) {
      env => ref =>
        Stitching
          .turn(env.classifier, env.reads, env.db.as(subject(ref)), ref.turn, env.tuning)
          .left
          .map(e => s"room unread: ${describe(e)}")
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
      .mayEnd(Flow.pass[WorkflowId].map { id =>
        StitchRef.fromWorkflowId(id).toRight(s"not a stitch: ${WorkflowId.value(id)}")
      })
      .andThen(Flow.decide(stitch) {
        case (ref, Right(Some((root, placed)))) => Right(Made(ref, root, placed))
        case (_, Right(None)) => Left("nothing asked")
        case (_, Left(why)) => Left(s"failed: $why")
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
