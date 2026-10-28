package grit.core.id

/** A schedule's key among those its [[Declarer]] declares: lowercase letters, digits and
  * dashes, starting with a letter.
  */
opaque type ScheduleKey = String

object ScheduleKey {

  /** The key `text` is, or why it is none. */
  def of(text: String): Either[String, ScheduleKey] =
    Slug
      .check("a schedule's key", text, None, leadingDigit = false)
      .left
      .map(_ => "a schedule's key is lowercase letters, digits and dashes, starting with a letter")

  def value(key: ScheduleKey): String = key
}
