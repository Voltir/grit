package grit.core.store

import grit.core.id.{ConversationId, EntrySeq}
import grit.core.place.Place

/** What a window shows from another conversation, under the place it happened in. Every seq a
  * section holds is one of `conversation`'s.
  */
enum Nearby {

  /** The entries of its open period at `entries`, in that order. */
  case Open(conversation: ConversationId, place: Place, entries: Vector[EntrySeq])

  /** One of its closed periods, by its closing entry. */
  case Closed(conversation: ConversationId, place: Place, closing: EntrySeq)

  /** Its entries at `entries`, a conversation in the window's own strand
    * ([[grit.core.stitch.Strand]]), in the order said: context the window's conversation
    * continues, shown unranked, not recall.
    */
  case Along(conversation: ConversationId, place: Place, entries: Vector[EntrySeq])

  /** The entries at `entries` of the turn whose hosted call made the post the window's
    * conversation begins with ([[Payload.Posted]]): why the post was made, shown whatever it
    * ranks.
    */
  case Asked(conversation: ConversationId, place: Place, entries: Vector[EntrySeq])

  def conversation: ConversationId

  def place: Place

  /** Every entry it names: an open, strand or asked section's entries, a closed one's
    * closing.
    */
  def names: Vector[EntrySeq] = this match {
    case Open(_, _, entries) => entries
    case Along(_, _, entries) => entries
    case Asked(_, _, entries) => entries
    case Closed(_, _, closing) => Vector(closing)
  }
}

object Nearby {

  /** The entries `sections` name that still exist, read from `store` once per conversation:
    * each conversation's in the order it is first named, ascending by seq within it. One gone
    * (purged with its period) is left out.
    */
  def read(sections: Vector[Nearby], store: EntryStore)(using
      Tx^
  ): Either[StoreError, Vector[Entry]] =
    sections
      .map(_.conversation)
      .distinct
      .foldLeft[Either[StoreError, Vector[Entry]]](Right(Vector.empty)) { (acc, c) =>
        acc.flatMap(found =>
          store.at(c, sections.filter(_.conversation == c).flatMap(_.names)).map(found ++ _)
        )
      }
}
