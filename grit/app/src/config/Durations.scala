package grit.app.config

import scala.concurrent.duration.{DurationLong, FiniteDuration}

/** Durations as a person writes them in a setting: a whole number and its unit, `s`, `m`, `h`
  * or `d` (`30s`, `3m`, `24h`, `30d`), spaces around either ignored.
  */
object Durations {

  private val Written = """\s*(\d{1,9})\s*([smhd])\s*""".r

  /** The duration `text` writes, or why it writes none. */
  def read(text: String): Either[String, FiniteDuration] = text match {
    case Written(n, unit) =>
      val count = n.toLong
      Right(unit match {
        case "s" => count.seconds
        case "m" => count.minutes
        case "h" => count.hours
        case _ => count.days
      })
    case _ => Left("not a duration: write a whole number and s, m, h or d, as 30s or 3m")
  }
}
