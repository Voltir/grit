package grit.core.store

import grit.core.id.{EntryId, Href}

/** Who wrote inbound entries, by the name each one's account goes by, and the link each was
  * recorded with when its edge gave one ([[Principals]]): what a window needs to say whose a
  * person's message is and where to open it. An entry not among them has neither.
  */
opaque type Speakers = (Map[EntryId, String], Map[EntryId, Href])

object Speakers {

  /** Nobody named, nothing linked: a window shows every message as it is. */
  val none: Speakers = (Map.empty, Map.empty)

  /** `names`, with no links. */
  def apply(names: Map[EntryId, String]): Speakers = (names, Map.empty)

  def apply(names: Map[EntryId, String], links: Map[EntryId, Href]): Speakers = (names, links)

  extension (s: Speakers) {

    /** The name of whoever wrote `entry`, if their account has one. */
    def of(entry: EntryId): Option[String] = s._1.get(entry)

    /** The link `entry` was recorded with, if its edge gave one. */
    def link(entry: EntryId): Option[Href] = s._2.get(entry)

    /** The union; an entry named or linked by both is one recorded message, so they agree. */
    def ++(other: Speakers): Speakers = (s._1 ++ other._1, s._2 ++ other._2)
  }
}
