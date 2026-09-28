package grit.core.spend

import java.time.{Instant, LocalDate, ZoneId}

/** A calendar day as `zone`'s clocks show it. */
final case class Day(date: LocalDate, zone: ZoneId) {

  /** Its first instant: midnight, or, when a clock change skips midnight, the first time the
    * day's clocks show.
    */
  def from: Instant = date.atStartOfDay(zone).toInstant

  /** The next day's first instant; not part of this day. */
  def until: Instant = date.plusDays(1).atStartOfDay(zone).toInstant
}

object Day {

  /** The day `at` falls on in `zone`. */
  def at(at: Instant, zone: ZoneId): Day = Day(LocalDate.ofInstant(at, zone), zone)
}
