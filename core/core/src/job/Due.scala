package grit.core.job

import java.time.{Duration, Instant}

/** What a schedule whose next slot is at `next` has due at `now`. */
enum Due {
  case NotYet

  /** Run the slot at `nominal`, the latest at or before `now`: a recurrence's earlier ones,
    * missed while grit was down, are never run. `following` is the slot after it, `None` for a
    * once slot.
    */
  case Run(nominal: Instant, following: Option[Instant])

  /** A once slot more than its grace past: recorded as missed, never run. */
  case Missed(nominal: Instant)
}

object Due {

  // Longer than any recurrence's period, a week, with room for a clock change.
  private val Lookback = Duration.ofDays(8)

  def of(rule: SlotRule, next: Instant, now: Instant): Due =
    if (next.isAfter(now)) NotYet
    else
      rule match {
        case SlotRule.Once(_, grace) =>
          val late = Duration.between(next, now)
          if (late.compareTo(Duration.ofNanos(Grace.value(grace).toNanos)) <= 0) Run(next, None)
          else Missed(next)
        case SlotRule.Daily(_, _) | SlotRule.Weekdays(_, _) | SlotRule.Weekly(_, _, _) =>
          val from = if (next.isBefore(now.minus(Lookback))) now.minus(Lookback) else next
          val nominal = Iterator
            .iterate(rule.after(from))(_.flatMap(rule.after))
            .takeWhile(_.exists(!_.isAfter(now)))
            .flatten
            .foldLeft(next)((_, slot) => slot)
          Run(nominal, rule.after(nominal))
      }
}
