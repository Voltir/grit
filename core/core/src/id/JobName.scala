package grit.core.id

/** A job's name (ADR 0029), which names its task place `task:{name}` and every schedule of
  * it: lowercase letters, digits and dashes, starting with a letter.
  */
opaque type JobName = String

object JobName {

  private val Valid = "[a-z][a-z0-9-]*".r

  /** The name `text` is, or why it is none. */
  def of(text: String): Either[String, JobName] =
    Either.cond(
      Valid.matches(text),
      text,
      "a job's name is lowercase letters, digits and dashes, starting with a letter"
    )

  def value(name: JobName): String = name
}
