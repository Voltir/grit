package grit.knowledge.sync

import grit.core.id.ScheduleKey
import grit.core.job.SlotRule

/** One sync schedule of a card source: its key among the source's schedules, its slot rule,
  * and at most how many summaries each of its runs asks for, the rest left to the next.
  */
final case class SyncSchedule private (key: ScheduleKey, rule: SlotRule, perRun: Int)
    extends caps.Pure

object SyncSchedule {

  /** These, or why not: `perRun` under 1. */
  def of(key: ScheduleKey, rule: SlotRule, perRun: Int): Either[String, SyncSchedule] =
    Either.cond(
      perRun >= 1,
      new SyncSchedule(key, rule, perRun),
      s"perRun is at least 1, not $perRun"
    )
}
