package grit.core.store

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}

/** What each model response cost, one row per response, kept while its period's closing is. */
trait UsageLedger {

  /** Records `usage` of the response held by entry `entry`, made for `turn`, from the workflow
    * `workflow`, beside `estimatedInput`: what grit estimated the request's input would cost,
    * so the estimate can be checked against the provider's `usage.input`. A close's cost is
    * made for its period's last turn but kept under the close's workflow, so [[of]] that
    * turn's workflow leaves it out. Once per entry: a second record of the same entry is
    * `DuplicateId`.
    */
  def record(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      model: String,
      usage: Usage,
      estimatedInput: Tokens
  )(using Tx^): Either[StoreError, Unit]

  /** The rows workflow `workflow` recorded, in the order it recorded them. */
  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Vector[UsageLedger.Row]]

  /** Deletes the rows recorded for `conversation`'s turns `from` to `to`, both included, and
    * the speech decisions on those turns ([[grit.core.speech.SpeechStore]]), which are kept
    * as long as their usage.
    */
  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      Tx^
  ): Either[StoreError, Unit]
}

object UsageLedger {

  /** One response's cost: the entry that holds it, the model that gave it, its `usage`,
    * and grit's estimate of its request's input.
    */
  final case class Row(entry: EntryId, model: String, usage: Usage, estimatedInput: Tokens)
}
