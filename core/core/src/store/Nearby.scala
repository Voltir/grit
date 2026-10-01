package grit.core.store

import grit.core.id.{ConversationId, EntryId}
import grit.core.place.Place

/** What a window shows from another conversation, under the place it happened in. */
enum Nearby {

  /** `entries` of its open period, in its order. */
  case Open(conversation: ConversationId, place: Place, entries: Vector[EntryId])

  /** `closing`, the closing entry of one of its closed periods. */
  case Closed(conversation: ConversationId, place: Place, closing: EntryId)

  /** `entries` of a conversation in the window's own strand ([[grit.core.stitch.Strand]]), in
    * the order said: context the window's conversation continues, shown unranked, not recall.
    */
  case Along(conversation: ConversationId, place: Place, entries: Vector[EntryId])

  /** `entries` of the turn whose hosted call made the post the window's conversation begins
    * with ([[Payload.Posted]]): why the post was made, shown whatever it ranks.
    */
  case Asked(conversation: ConversationId, place: Place, entries: Vector[EntryId])

  def conversation: ConversationId

  def place: Place

  /** Every entry it names: an open, strand or asked section's entries, a closed one's
    * closing.
    */
  def names: Vector[EntryId] = this match {
    case Open(_, _, entries) => entries
    case Along(_, _, entries) => entries
    case Asked(_, _, entries) => entries
    case Closed(_, _, closing) => Vector(closing)
  }
}
