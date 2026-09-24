package grit.core

/** Capability to run the current workflow's durable operations. Each completes at most
  * once per workflow id: its output is recorded, and a restarted workflow gets the
  * recorded output back instead of running the body again. Operations must be called in
  * the same order, with the same names, on every run of a workflow.
  *
  * Exclusive (ADR 0003): a body may capture any other capability, but not the `Durable`
  * it runs on, so a step inside a step is a compile error.
  *
  * A body that throws has its exception recorded instead, and every later run rethrows it
  * without running the body: expected failure belongs in `A` (STYLE rule 3). Only a body
  * cut short by a crash runs again.
  *
  * The workflow's id is not an operation, so it is not here: a workflow body receives it
  * as a plain argument, which a step body may capture.
  */
trait Durable extends caps.ExclusiveCapability {

  /** Runs `body` once for this workflow, as the step `name`, and records its output. */
  def step[A: Journaled](name: String)(body: () => A): A

  /** As [[step]], inside a database transaction that commits atomically with the record
    * of its output. The `Tx` is valid only within `body`.
    */
  def transact[A: Journaled](name: String)(body: (Tx^) ?=> A): A

  /** Whether this run takes the new branch of the change `name`: true for a workflow that
    * reaches this point after the change shipped, false for one that had already passed
    * it. Guard the changed steps with it, keeping the old ones on the false branch, so a
    * workflow in flight across the change replays the steps it recorded.
    */
  def patch(name: String): Boolean

  /** Retires the patch `name` once no workflow in flight took its false branch: replaces
    * `if (patch(name)) new else old` with `deprecatePatch(name); new`. Workflows that
    * recorded the patch still replay; new ones no longer record it.
    */
  def deprecatePatch(name: String): Unit
}
