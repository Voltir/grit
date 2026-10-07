package grit.core.store

import grit.core.id.EntryId

/** Who wrote inbound entries, by the name each one's account goes by ([[Principals]]): what a
  * window needs to say whose a person's message is. An entry not among them is shown with no
  * name.
  */
opaque type Speakers = Map[EntryId, String]

object Speakers {

  /** Nobody named: a window shows every message as it is. */
  val none: Speakers = Map.empty

  def apply(names: Map[EntryId, String]): Speakers = names

  extension (s: Speakers) {

    /** The name of whoever wrote `entry`, if their account has one. */
    def of(entry: EntryId): Option[String] = s.get(entry)
  }
}
