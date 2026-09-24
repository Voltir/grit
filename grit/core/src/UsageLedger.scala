package grit.core

/** What each model response cost, one row per response, kept for the life of the
  * database (conversations expire; spend does not).
  */
trait UsageLedger {

  /** Records `usage` of the response held by entry `entry`, from the workflow `workflow`.
    * Once per entry: a second record of the same entry is `DuplicateId`.
    */
  def record(entry: EntryId, workflow: WorkflowId, model: String, usage: Usage)(using
      Tx^
  ): Either[StoreError, Unit]
}
