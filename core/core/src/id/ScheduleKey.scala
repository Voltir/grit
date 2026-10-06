package grit.core.id

/** A schedule's key among those its [[Declarer]] declares: lowercase letters, digits and
  * dashes, starting with a letter.
  */
opaque type ScheduleKey = String

object ScheduleKey {

  private val Valid = "[a-z][a-z0-9-]*".r

  /** The key `text` is, or why it is none. */
  def of(text: String): Either[String, ScheduleKey] =
    Either.cond(
      Valid.matches(text),
      text,
      "a schedule's key is lowercase letters, digits and dashes, starting with a letter"
    )

  def value(key: ScheduleKey): String = key
}
