package grit.core.stitch

import grit.core.id.ConversationId
import grit.core.place.Place
import grit.core.store.Entry

/** `conversation`'s first message continues the exchange `root` began. */
final case class Link(conversation: ConversationId, root: ConversationId)

/** An entry of a conversation other than the reader's, with the place it was said at. */
final case class Said(conversation: ConversationId, place: Place, entry: Entry)

object Strand {

  /** The strand of `conversation` among `links`: the root it follows (itself when it follows
    * none) and every conversation that follows that root. One hop: a follower of a follower is
    * not in it.
    */
  def of(conversation: ConversationId, links: Vector[Link]): Set[ConversationId] = {
    val root = links.find(_.conversation == conversation).fold(conversation)(_.root)
    links.filter(_.root == root).map(_.conversation).toSet + root
  }

  /** What a reader is shown of its strand ([[Along.read]]): the root's `opening` message when
    * the root is another conversation and its first message is still kept; what the other
    * members `said` in the range read, oldest first (the opening among them when it was said
    * in range); the members whose first message is `gone` (their raw entries purged), shown by
    * their record instead; and every member but the reader (`conversations`), whether or not
    * anything of theirs is shown.
    */
  final case class Read(
      opening: Option[Said],
      said: Vector[Said],
      gone: Vector[ConversationId],
      conversations: Set[ConversationId]
  ) {

    /** Every entry it shows: the opening, then what was said, each once. */
    def shown: Vector[grit.core.store.Entry] =
      (opening.toVector ++ said).map(_.entry).distinctBy(_.id)

    /** Every conversation it shows messages of. */
    def members: Vector[ConversationId] =
      (opening.map(_.conversation).toVector ++ said.map(_.conversation)).distinct
  }

  object Read {
    val empty: Read = Read(None, Vector.empty, Vector.empty, Set.empty)
  }
}
