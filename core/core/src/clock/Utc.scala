package grit.core.clock

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZoneOffset}
import java.util.Locale

/** An instant as grit writes it for a model or a person to read: in UTC, said so, or in a zone,
  * named by its short name.
  */
object Utc {

  private val Second =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss 'UTC'").withZone(ZoneOffset.UTC)

  private val Minute =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm 'UTC'").withZone(ZoneOffset.UTC)

  /** `at` to the second: `2026-10-06 14:00:05 UTC`. */
  def toSecond(at: Instant): String = Second.format(at)

  /** `at` to the minute, its seconds dropped: `2026-10-06 14:00 UTC`. */
  def toMinute(at: Instant): String = Minute.format(at)

  private val DayMinute =
    DateTimeFormatter
      .ofPattern("EEE yyyy-MM-dd HH:mm 'UTC'", Locale.ROOT)
      .withZone(ZoneOffset.UTC)

  /** `at` to the minute after its weekday, so "today" and "Wednesday" can be read against it:
    * `Tue 2026-10-06 14:00 UTC`.
    */
  def toDayMinute(at: Instant): String = DayMinute.format(at)

  // US English, whose short zone names are the ones people write (PDT, not GMT-07:00).
  private val ZonedDayMinute = DateTimeFormatter.ofPattern("EEE yyyy-MM-dd HH:mm zzz", Locale.US)

  /** `at` to the minute after its weekday in `zone`, its zone's short name after: `Wed 2026-10-21
    * 08:03 PDT`.
    */
  def toDayMinute(at: Instant, zone: ZoneId): String = ZonedDayMinute.withZone(zone).format(at)

  private val ShortName = DateTimeFormatter.ofPattern("zzz", Locale.US)

  /** `zone`'s short name at `at`, as [[toDayMinute]] writes it after the minute: `PDT`, or `PST`
    * in winter.
    */
  def shortName(at: Instant, zone: ZoneId): String = ShortName.withZone(zone).format(at)

  private val TimeOfDay = DateTimeFormatter.ofPattern("HH:mm 'UTC'").withZone(ZoneOffset.UTC)

  /** `at`'s time of day to the minute, without its date: `15:03 UTC`. */
  def toClock(at: Instant): String = TimeOfDay.format(at)
}
