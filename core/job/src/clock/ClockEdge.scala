package grit.job.clock

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.id.ScheduleId
import grit.core.inbox.{Inbox, InboxError, Slotted}
import grit.core.job.{Jobs, ScheduleStore}
import grit.core.store.{Db, StoreError}
import grit.core.visibility.Subject

/** grit's clock edge (ADR 0029): starts what each schedule has waiting at its clock's time
  * through the inbox, as any edge starts a turn (ADR 0002); hosts no place.
  */
final class ClockEdge(inbox: Inbox, schedules: ScheduleStore, db: Db, clock: Clock, jobs: Jobs)
    extends caps.SharedCapability {

  /** One pass: every schedule with a run in flight and every one with a slot due at now, at
    * most [[ClockEdge.Batch]] of each, so neither kind holds back the other, each started at its
    * job's version ([[Inbox.startSlot]]). `Left` when the schedules cannot be read; one the inbox
    * fails is named in [[Ticked.failed]] and tried again next pass.
    */
  def tick(): Either[StoreError, Ticked] = {
    val now = clock.now()
    db.read(Subject.Public) {
      for {
        flying <- schedules.inFlight(ClockEdge.Batch)
        due <- schedules.due(now, ClockEdge.Batch)
      } yield flying ++ due
    }.map { waiting =>
      val tried = waiting.map { (id, job) =>
        id -> inbox.startSlot(id, jobs.named(job).map(_.version), now)
      }
      Ticked(
        tried.collect { case (id, Right(slotted)) => id -> slotted },
        tried.collect { case (id, Left(error)) => id -> error }
      )
    }
  }
}

object ClockEdge {

  /** How often grit's engine runs a pass: 5 s. */
  val Every: FiniteDuration = 5.seconds

  /** The most schedules of each kind, due or in flight, one pass takes up: 100. */
  val Batch: Int = 100
}

/** What a pass did: each schedule it started, as the inbox says, and each the inbox failed. */
final case class Ticked(
    slotted: Vector[(ScheduleId, Slotted)],
    failed: Vector[(ScheduleId, InboxError)]
)
