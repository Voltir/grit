package grit.core.store

import grit.core.id.{EntryId, WorkflowId}
import grit.core.message.{Tokens, Usage}

/** What each model response cost, one row per response, kept for the life of the
  * database (conversations expire; spend does not).
  */
trait UsageLedger {

  /** Records `usage` of the response held by entry `entry`, from the workflow `workflow`,
    * beside `estimatedInput`: what grit estimated the request's input would cost, so the
    * estimate can be checked against the provider's `usage.input`. Once per entry: a
    * second record of the same entry is `DuplicateId`.
    */
  def record(
      entry: EntryId,
      workflow: WorkflowId,
      model: String,
      usage: Usage,
      estimatedInput: Tokens
  )(using Tx^): Either[StoreError, Unit]
}
