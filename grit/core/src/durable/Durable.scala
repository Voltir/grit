package grit.core.durable

import scala.concurrent.duration.FiniteDuration

import grit.core.store.Tx

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

  /** The stream `key` of this workflow: pieces an edge reads through the database as they
    * are written, beside the recorded outputs and never among them. Take it before a step
    * and write it inside one: a write is not an operation, so it neither replays nor
    * shifts the steps after it. At least once: a step cut short by a crash and run again
    * writes its pieces again, after the first run's, so a piece should say which run
    * wrote it.
    */
  def stream(key: String): StreamWriter

  /** The oldest message sent to this workflow on `topic` that it has not received, waiting
    * up to `timeout` for one; `None` when none came in time. What came, or that nothing did,
    * is recorded, so a replay returns it at once. The wait's end is recorded as it begins:
    * a workflow resumed after a crash waits only for what was left of it.
    */
  def recv(topic: String, timeout: FiniteDuration): Option[String]
}

/** Where a step tells an edge what it is doing while it does it ([[Durable.stream]]). */
trait StreamWriter extends caps.SharedCapability {

  /** Appends `piece` to the stream. Only inside a step. */
  def write(piece: String): Unit
}
