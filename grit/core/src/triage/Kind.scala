package grit.core.triage

/** What a heard message is, as triage reads it. */
enum Kind {
  case Question, Answer, Decision, Announcement, Chatter
}

object Kind {

  /** `kind`'s stored name, its own in lower case: the same in `grit.triage` and a triage's
    * journal.
    */
  def written(kind: Kind): String = kind.toString.toLowerCase

  /** The kind stored as `name`; `None` for no kind's. */
  def read(name: String): Option[Kind] = values.find(written(_) == name)
}
