package grit.lifecycle.stitch

import grit.core.durable.Durable
import grit.core.id.{EntryId, StitchRef, WorkflowId}
import grit.core.stitch.{StitchJson, Stitching}
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

  /** The placement's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Stitch = "stitch"
    val RecordStitch = "record-stitch"
  }

  /** The placement workflow for the opening whose workflow id is `workflowId`. What it did,
    * for logs: the placement is in the store.
    */
  def body(env: StitchEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    StitchRef.fromWorkflowId(workflowId) match {
      case None => s"not a stitch: ${WorkflowId.value(workflowId)}"
      case Some(stitch) =>
        import StitchJournal.given
        val (classifier, reads, tuning) = (env.classifier, env.reads, env.tuning)
        val subject = Subject.Conversation(stitch.turn.conversationId)
        val db = env.db.as(subject)
        d.step(Step.Stitch)(() =>
          Stitching
            .turn(classifier, reads, db, stitch.turn, tuning)
            .left
            .map(e => s"room unread: ${describe(e)}")
        ) match {
          case Right(Some((root, placed))) =>
            val at = env.clock.now()
            d.transact(Step.RecordStitch, subject)(
              reads.stitches.record(root, placed, at).left.map(describe)
            ) match {
              case Right(true) => s"stitched: ${StitchJson.kindOf(placed)}"
              case Right(false) => "not kept: the opening is gone or placed already"
              case Left(why) => s"not kept: $why"
            }
          case Right(None) => "nothing asked"
          case Left(why) => s"failed: $why"
        }
    }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
