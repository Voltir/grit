package grit.core.job

import java.time.Instant

import grit.core.id.{Declarer, JobName, PrincipalId, ScheduleId}
import grit.core.store.{StoreError, Tx}

/** A stored schedule: its job, its parameters as kept, whom it runs for, where it reports, its
  * rule, and how it ended (`None` while pending).
  */
final case class Schedule(
    job: JobName,
    params: ujson.Value,
    principal: PrincipalId,
    report: Report,
    rule: SlotRule,
    ended: Option[Ending]
)

/** The stored schedules, as the engine and its clock edge keep them. Every schedule ended is
  * marked for deletion as it ends ([[grit.core.retention.Target.Schedule]]).
  */
trait ScheduleStore {

  /** Makes the stored declared schedules `declared`, by their declarers, as of `now`. A new one
    * is written with its first slot after `now` (a once slot at its instant, however past). A
    * changed one (its job, parameters or rule) is rewritten in place, its next slot recomputed
    * only when its rule changed. One ended undeclared is revived with the first slot after
    * `now`, as a new one is; one ended any other way stays ended. Every pending declared one not among them is ended undeclared. An asked schedule is
    * never touched.
    */
  def declare(declared: Vector[(Declarer, Declared[?])], now: Instant)(using
      Tx^
  ): Either[StoreError, Unit]

  /** The pending schedules with a slot due at `now` or a run not yet replied, with their jobs,
    * at most `n`: the soonest first, one with a run in flight by its run's slot, ties by id.
    */
  def waiting(now: Instant, n: Int)(using Tx^): Either[StoreError, Vector[(ScheduleId, JobName)]]

  /** `slot`'s run at `version` replied at `at`: its run no longer in flight, and a once schedule
    * not already ended ended [[Ending.Ran]]. Nothing when the run in flight is another's, of
    * another slot or another version (that run is superseded).
    */
  def replied(slot: Slot, version: Int, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** `id`, or `None` when it is gone. */
  def read(id: ScheduleId)(using Tx^): Either[StoreError, Option[Schedule]]
}
