package grit.core.store

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.model.Grade

/** What each model response cost, one row per response, kept while its period's closing is. */
trait UsageLedger {

  /** Records `usage` of the response held by entry `entry`, made for `turn`, from the workflow
    * `workflow`, beside `estimatedInput`: what grit estimated the request's input would cost,
    * so the estimate can be checked against the provider's `usage.input`. A close's cost is
    * made for its period's last turn but kept under the close's workflow, so [[of]] that
    * turn's workflow leaves it out. `upstream` is who served it, as its provider reported;
    * `None` when it did not. `grade` is the grade of the pin the call was made under; `None`
    * when it was made under none. Once per entry: a second record of the same entry is
    * `DuplicateId`.
    */
  def record(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      model: String,
      upstream: Option[String],
      grade: Option[Grade],
      usage: Usage,
      estimatedInput: Tokens
  )(using Tx^): Either[StoreError, Unit]

  /** The rows workflow `workflow` recorded, in the order it recorded them. */
  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Vector[UsageLedger.Row]]

  /** Deletes the rows recorded for `conversation`'s turns `from` to `to`, both included. */
  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      Tx^
  ): Either[StoreError, Unit]
}

object UsageLedger {

  /** One response's cost: the entry that holds it, the model that gave it, the upstream that
    * served it (`None`: not reported), the grade of the pin it was made under (`None`: not made
    * under a pin, or recorded before grades were), its `usage`, and grit's estimate of its
    * request's input.
    */
  final case class Row(
      entry: EntryId,
      model: String,
      upstream: Option[String],
      grade: Option[Grade],
      usage: Usage,
      estimatedInput: Tokens
  )
}
