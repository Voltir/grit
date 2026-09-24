package grit.core

/** An in-memory [[UsageLedger]] for tests. It ignores the `Tx`. */
final class InMemoryUsageLedger extends UsageLedger {

  @caps.unsafe.untrackedCaptures
  var rows = Vector.empty[(EntryId, WorkflowId, String, Usage)]

  def record(entry: EntryId, workflow: WorkflowId, model: String, usage: Usage)(using
      Tx^
  ): Either[StoreError, Unit] =
    if (rows.exists(_._1 == entry)) Left(StoreError.DuplicateId(entry))
    else {
      rows = rows :+ (entry, workflow, model, usage)
      Right(())
    }
}
