package grit.core.job

import java.time.{DayOfWeek, Instant, LocalTime, ZoneId, ZonedDateTime}

/** When a schedule's slots fall (ADR 0029): once, or a recurrence from this closed set at a
  * local time in a zone; never a cron expression. A local time a clock change skips falls
  * later by the gap's length (02:30 in a one-hour gap at 03:30); one it repeats, at its first
  * instance (java.time's rules).
  */
enum SlotRule {
  case Once(at: Instant, grace: Grace)
  case Daily(at: LocalTime, zone: ZoneId)

  /** Monday to Friday. */
  case Weekdays(at: LocalTime, zone: ZoneId)
  case Weekly(day: DayOfWeek, at: LocalTime, zone: ZoneId)

  /** Its first slot strictly after `instant`; `None` for a `Once` whose instant is not after
    * it.
    */
  def after(instant: Instant): Option[Instant] = this match {
    case Once(at, _) => Option.when(at.isAfter(instant))(at)
    case Daily(at, zone) => SlotRule.firstAfter(instant, at, zone, _ => true)
    case Weekdays(at, zone) =>
      SlotRule.firstAfter(instant, at, zone, d => d != DayOfWeek.SATURDAY && d != DayOfWeek.SUNDAY)
    case Weekly(day, at, zone) => SlotRule.firstAfter(instant, at, zone, _ == day)
  }

  /** The first slot of a schedule made at `now`: a once slot's own instant, however far past
    * (whether it then runs or is missed is its grace's to say, [[Due]]); a recurrence's first
    * slot after `now`.
    */
  def first(now: Instant): Option[Instant] = this match {
    case Once(at, _) => Some(at)
    case Daily(_, _) | Weekdays(_, _) | Weekly(_, _, _) => after(now)
  }
}

object SlotRule {

  // A week and a day from the instant's local date holds the next slot of every recurrence.
  private def firstAfter(
      instant: Instant,
      at: LocalTime,
      zone: ZoneId,
      falls: DayOfWeek => Boolean
  ): Option[Instant] = {
    val from = instant.atZone(zone).toLocalDate
    (0 to 8).iterator
      .map(from.plusDays(_))
      .filter(d => falls(d.getDayOfWeek))
      .map(d => ZonedDateTime.of(d, at, zone).toInstant)
      .find(_.isAfter(instant))
  }
}
