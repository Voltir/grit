package grit.core.job

import java.time.Instant

import grit.core.id.{Declarer, JobName, PrincipalId, ScheduleId}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** A stored schedule: its job, its parameters as kept, whom it runs for, where it reports, its
  * rule, how it ended (`None` while pending), and its label: what its runs read beyond their
  * own conversations, and are kept at (ADR 0030): an asked one's, its asking turn's floor
  * ([[ScheduleDesk.ask]]); a declared one's, its [[Declared.clearance]].
  */
final case class Schedule(
    job: JobName,
    params: ujson.Value,
    principal: PrincipalId,
    report: Report,
    rule: SlotRule,
    ended: Option[Ending],
    label: Label
)

/** The stored schedules, as the engine and its clock edge keep them. Every schedule ended is
  * marked for deletion as it ends ([[grit.core.retention.Target.Schedule]]).
  */
trait ScheduleStore {

  /** Makes the stored declared schedules `declared`, by their declarers, as of `now`. A new one
    * is written with its first slot after `now` (a once slot at its instant, however past),
    * kept as [[Slot.kept]] keeps it. A changed one (its job, parameters, rule or clearance) is
    * rewritten in place, its next slot recomputed only when its rule changed. One ended undeclared is revived
    * with the first slot after `now`, as a new one is; one ended any other way stays ended.
    * Every pending declared one not among them is ended undeclared. An asked schedule is never
    * touched.
    */
  def declare(declared: Vector[(Declarer, Declared[?])], now: Instant)(using
      Tx^
  ): Either[StoreError, Unit]

  /** The pending schedules with no run in flight and a slot due at `now`, with their jobs, at
    * most `n`: the soonest slot first, ties by id.
    */
  def due(now: Instant, n: Int)(using Tx^): Either[StoreError, Vector[(ScheduleId, JobName)]]

  /** The pending schedules with a run not yet replied, whatever their next slot, with their
    * jobs, at most `n`: the earliest run's slot first, ties by id.
    */
  def inFlight(n: Int)(using Tx^): Either[StoreError, Vector[(ScheduleId, JobName)]]

  /** `slot`'s run at `version` replied at `at`: its run no longer in flight, and a once schedule
    * not already ended ended [[Ending.Ran]]. Nothing when the run in flight is another's, of
    * another slot or another version (that run is superseded).
    */
  def replied(slot: Slot, version: Int, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** `id`, or `None` when it is gone. */
  def read(id: ScheduleId)(using Tx^): Either[StoreError, Option[Schedule]]
}
