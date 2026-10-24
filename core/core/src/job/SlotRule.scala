package grit.core.job

import java.time.{DayOfWeek, Duration, Instant, LocalTime, ZoneId, ZonedDateTime}

/** When a schedule's slots fall (ADR 0029): once, or a recurrence from this closed set, at a
  * local time in a zone or every so long; never a cron expression. A local time a clock change skips falls
  * later by the gap's length (02:30 in a one-hour gap at 03:30); one it repeats, at its first
  * instance (java.time's rules).
  */
enum SlotRule {
  case Once(at: Instant, grace: Grace)
  case Daily(at: LocalTime, zone: ZoneId)

  /** Monday to Friday. */
  case Weekdays(at: LocalTime, zone: ZoneId)
  case Weekly(day: DayOfWeek, at: LocalTime, zone: ZoneId)

  /** At each whole multiple of `period` since the epoch, 1970-01-01T00:00Z: the same slots in
    * any zone and across restarts (every 30 minutes falls at :00 and :30 UTC). Its `period` is
    * a length of time, not a conversation's [[grit.core.period.Period]].
    */
  case Every(period: Interval)

  /** Its slots after `from` and at or before `until`, in order. */
  def between(from: Instant, until: Instant): Iterator[Instant] =
    Iterator.iterate(after(from))(_.flatMap(after)).takeWhile(_.exists(!_.isAfter(until))).flatten

  /** Its first slot strictly after `instant`; `None` for a `Once` whose instant is not after
    * it.
    */
  def after(instant: Instant): Option[Instant] = this match {
    case Once(at, _) => Option.when(at.isAfter(instant))(at)
    case Daily(at, zone) => SlotRule.firstAfter(instant, at, zone, _ => true)
    case Weekdays(at, zone) =>
      SlotRule.firstAfter(instant, at, zone, d => d != DayOfWeek.SATURDAY && d != DayOfWeek.SUNDAY)
    case Weekly(day, at, zone) => SlotRule.firstAfter(instant, at, zone, _ == day)
    case Every(period) => Some(SlotRule.nextMultiple(instant, period))
  }

  /** The first slot of a schedule made at `now`: a once slot's own instant, however far past
    * (whether it then runs or is missed is its grace's to say, [[Due]]); a recurrence's first
    * slot after `now`.
    */
  def first(now: Instant): Option[Instant] = this match {
    case Once(at, _) => Some(at)
    case Daily(_, _) | Weekdays(_, _) | Weekly(_, _, _) | Every(_) => after(now)
  }

  /** Whether a schedule that ended under `was` is pending again when declared as this: `was` a
    * once rule, and this a once rule at another instant or any recurrence, each a new slot. The
    * same instant, a grace alone changed, or a `was` that is a recurrence is not.
    */
  def rearms(was: SlotRule): Boolean = (was, this) match {
    case (Once(before, _), Once(after, _)) => before != after
    case (Once(_, _), Daily(_, _) | Weekdays(_, _) | Weekly(_, _, _) | Every(_)) => true
    case (Daily(_, _) | Weekdays(_, _) | Weekly(_, _, _) | Every(_), _) => false
  }
}

object SlotRule {

  private def nextMultiple(instant: Instant, period: Interval): Instant = {
    val p = Duration.ofNanos(Interval.value(period).toNanos)
    // dividedBy truncates toward zero: the k-th multiple is after `instant` only before the
    // epoch, and the (k+1)-th always is.
    val k = Duration.between(Instant.EPOCH, instant).dividedBy(p)
    val kth = Instant.EPOCH.plus(p.multipliedBy(k))
    if (kth.isAfter(instant)) kth else kth.plus(p)
  }

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
