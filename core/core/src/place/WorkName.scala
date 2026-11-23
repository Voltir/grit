package grit.core.place

import grit.core.id.Slug

/** One run of work's name: a lowercase letter, then at most 39 lowercase letters, digits or `-`.
  * A place segment, a directory's name and a git ref.
  */
opaque type WorkName = String

object WorkName {

  /** `s` as a run of work's name, or why not. */
  def of(s: String): Either[String, WorkName] =
    Slug.check("a run of work's name", s, Some(40), leadingDigit = false)

  def value(work: WorkName): String = work
}
