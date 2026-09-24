package grit.core.store

import grit.core.id.{EntryId, WorkflowId}
import grit.core.message.{Tokens, Usage}

/** An in-memory [[UsageLedger]] for tests. It ignores the `Tx`. */
final class InMemoryUsageLedger extends UsageLedger {

  @caps.unsafe.untrackedCaptures
  var rows = Vector.empty[(EntryId, WorkflowId, String, Usage, Tokens)]

  def record(
      entry: EntryId,
      workflow: WorkflowId,
      model: String,
      usage: Usage,
      estimatedInput: Tokens
  )(using Tx^): Either[StoreError, Unit] =
    if (rows.exists(_._1 == entry)) Left(StoreError.DuplicateId(entry))
    else {
      rows = rows :+ (entry, workflow, model, usage, estimatedInput)
      Right(())
    }

  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Vector[UsageLedger.Row]] =
    Right(rows.collect {
      case (entry, w, model, usage, estimate) if w == workflow =>
        UsageLedger.Row(entry, model, usage, estimate)
    })
}
