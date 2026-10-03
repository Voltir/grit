package grit.core.triage

/** What a heard message is, as triage reads it. */
enum Kind {
  case Question, Answer, Decision, Announcement, Chatter
}

object Kind {

  /** `kind`'s key in v1's `kind` choice ([[Tags.V1]]), its own name in lower case: as
    * triage's answers hold it, and as an earlier build's journal recorded it.
    */
  def written(kind: Kind): String = kind.toString.toLowerCase

  /** The kind stored as `name`; `None` for no kind's. */
  def read(name: String): Option[Kind] = values.find(written(_) == name)
}
