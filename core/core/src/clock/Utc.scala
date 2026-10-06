package grit.core.clock

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

/** An instant as grit writes it for a model or a person to read: in UTC, said so. */
object Utc {

  private val Second =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss 'UTC'").withZone(ZoneOffset.UTC)

  private val Minute =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm 'UTC'").withZone(ZoneOffset.UTC)

  /** `at` to the second: `2026-10-06 14:00:05 UTC`. */
  def toSecond(at: Instant): String = Second.format(at)

  /** `at` to the minute, its seconds dropped: `2026-10-06 14:00 UTC`. */
  def toMinute(at: Instant): String = Minute.format(at)
}
