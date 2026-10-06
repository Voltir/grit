package grit.core.job

import java.time.Instant
import java.time.temporal.ChronoUnit

import scala.util.Try

import grit.core.clock.Utc
import grit.core.id.{JobName, ScheduleId, SourceId}
import grit.core.message.Message
import grit.core.store.Origin

/** One slot of a schedule (ADR 0029). Its run is a turn of the conversation `origin` names, so
  * the inbox, finding that conversation, never starts the slot twice at one version.
  */
final case class Slot(schedule: ScheduleId, nominal: Instant) {

  /** `{schedule}@{nominal, ISO-8601}`: a stored form, the run's `Origin.Task` run name. */
  def key: String = s"${ScheduleId.value(schedule)}@$nominal"

  def origin(job: JobName): Origin.Task = Origin.Task(JobName.value(job), key)

  /** The run's opening, written by grit: "Scheduled run of {job}, due {nominal}", the nominal
    * instant as [[Utc.toMinute]] writes it.
    */
  def opening(job: JobName): Message.User =
    Message.User(s"Scheduled run of ${JobName.value(job)}, due ${Utc.toMinute(nominal)}")
}

object Slot {

  private val Version = "v(0|-?[1-9][0-9]*)".r

  /** `instant` as a schedule keeps a slot's, asked or declared: truncated to the microsecond,
    * the store's precision, so a slot is never kept after the instant it was made for.
    */
  def kept(instant: Instant): Instant = instant.truncatedTo(ChronoUnit.MICROS)

  /** The slot whose [[Slot.key]] is `key`; `None` for any other run name (a one-shot run's
    * `main`).
    */
  def read(key: String): Option[Slot] =
    key.lastIndexOf('@') match {
      case -1 => None
      case i =>
        for {
          schedule <- ScheduleId.of(key.take(i)).toOption
          nominal <- Try(Instant.parse(key.drop(i + 1))).toOption
          slot = Slot(schedule, nominal)
          if slot.key == key
        } yield slot
    }

  /** The slot whose run `origin`'s conversation is: a task's run named by a [[Slot.key]];
    * `None` for any other conversation (a one-shot run's `main`, a person's).
    */
  def of(origin: Origin): Option[Slot] = origin match {
    case Origin.Task(_, run) => read(run)
    case Origin.Tui(_, _) | Origin.Slack(_, _, _) => None
  }

  /** The opening source of a run at `version`, `v{version}`, and back. */
  def source(version: Int): SourceId = SourceId(s"v$version")

  def version(source: SourceId): Option[Int] =
    SourceId.value(source) match {
      case Version(digits) => digits.toIntOption
      case _ => None
    }
}
