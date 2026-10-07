package grit.core.job

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Utc
import grit.core.id.{CallSlot, JobName, PluginName, ScheduleId}
import grit.core.store.StoreError

/** One asked schedule, as its asker sees it. */
final case class Asked[P <: caps.Pure](id: ScheduleId, at: Instant, params: P)

/** The asker's pending schedules of one job, as of `now`, soonest first. */
final case class Pending[P <: caps.Pure](now: Instant, schedules: Vector[Asked[P]])

/** The schedules one plugin's tools write from a turn (ADR 0029). Each is a once slot, run for
  * the asker, the author of the message the turn at the call answers; reported where that
  * turn's reply is posted, through the edge its conversation came by ([[Report.Posted]],
  * [[grit.core.store.Origin.edge]]); kept under the id its call makes
  * ([[grit.core.id.ScheduleId.asked]]); and the asker's alone to list and cancel. A desk holds
  * its plugin's job names, and refuses a booking of any other job with
  * [[DeskRefusal.NotOwn]]. Every method is [[DeskRefusal.Unavailable]] when the store fails,
  * and when a deployment's declaration merged the asker into another person while it ran;
  * asked again, it is that person's.
  */
trait ScheduleDesk extends caps.SharedCapability {

  /** Schedules `booking`'s job `when`, with `params`, to run as late as `grace` after its
    * instant, kept as [[Slot.kept]] keeps it ([[Asked.at]]), at the asking turn's floor
    * ([[grit.core.store.Tx.floor]] of a transaction opened for that turn), its room's label:
    * what its runs, and what they keep, are kept at. The same call asking again (a call
    * run twice after a crash) gets the schedule it first wrote, unchanged. Refused, writing
    * nothing:
    *   - [[DeskRefusal.Unaddressed]] when the turn's reply is posted nowhere (a TUI session, a
    *     draft grit has not posted), or the call's turn is not recorded;
    *   - `Past` when `when` is not after now;
    *   - `TooFar` when it is more than [[ScheduleDesk.Horizon]] after now;
    *   - `TooMany` when the asker has [[ScheduleDesk.PendingCap]] pending, of any job;
    *   - `NotOwn`.
    */
  def ask[P <: caps.Pure](
      call: CallSlot,
      booking: Booking[P],
      when: When,
      grace: Grace,
      params: P
  ): Either[DeskRefusal, Asked[P]]

  /** The asker's pending schedules of `booking`'s job that the call's turn reads, as anything
    * recorded in the room each was asked in: one asked in a room labelled above what the turn
    * reads beyond its own room is left out, as is one whose parameters its job cannot read;
    * none when the call's turn is not recorded.
    */
  def pending[P <: caps.Pure](call: CallSlot, booking: Booking[P]): Either[DeskRefusal, Pending[P]]

  /** Ends `id`, the asker's pending schedule of `booking`'s job, [[Ending.Cancelled]].
    * `NotFound` when no schedule of the asker's of that job that the call's turn reads (as
    * [[pending]] does) has it, whoever else's it is; `Ended` when it has already ended.
    */
  def cancel(call: CallSlot, booking: Booking[?], id: ScheduleId): Either[DeskRefusal, Unit]
}

object ScheduleDesk {

  /** The most schedules one person may have pending through every desk: 20. */
  val PendingCap: Int = 20

  /** The furthest ahead a schedule may be asked for: 366 days. */
  val Horizon: FiniteDuration = 366.days
}

/** Why a desk wrote nothing; [[said]] is the line a model is shown, its times as [[Utc.toSecond]] writes them. */
enum DeskRefusal {
  case Unaddressed
  case Past(at: Instant, now: Instant)
  case TooFar(at: Instant, limit: Instant)
  case TooMany(cap: Int)
  case NotOwn(plugin: PluginName, job: JobName)
  case NotFound(id: ScheduleId)
  case Ended(id: ScheduleId, how: Ending)
  case Unavailable(cause: StoreError)

  def said: String = this match {
    case Unaddressed =>
      "Nothing was scheduled: this conversation's replies are not posted anywhere a " +
        "scheduled run's reply could be."
    case Past(at, now) =>
      s"Nothing was scheduled: ${Utc.toSecond(at)} is not after now, ${Utc.toSecond(now)}."
    case TooFar(at, limit) =>
      s"Nothing was scheduled: ${Utc.toSecond(at)} is after ${Utc.toSecond(limit)}, the " +
        "furthest ahead one can be."
    case TooMany(cap) =>
      s"Nothing was scheduled: you already have $cap pending, the most one person may have."
    case NotOwn(plugin, job) =>
      s"Nothing was done: ${PluginName.value(plugin)} cannot schedule ${JobName.value(job)}."
    case NotFound(id) => s"None of your pending schedules has the id ${ScheduleId.value(id)}."
    case Ended(id, how) => s"${ScheduleId.value(id)} has already ended: ${how.word}."
    case Unavailable(_) => "The schedules could not be reached just now; nothing was done."
  }
}
