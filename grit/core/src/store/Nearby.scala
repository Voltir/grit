package grit.core.store

import grit.core.id.{ConversationId, EntryId}
import grit.core.place.Place

/** What a window shows from another conversation, under the place it happened in. */
enum Nearby {

  /** `entries` of its open period, in its order. */
  case Open(conversation: ConversationId, place: Place, entries: Vector[EntryId])

  /** `closing`, the closing entry of one of its closed periods. */
  case Closed(conversation: ConversationId, place: Place, closing: EntryId)

  def conversation: ConversationId

  def place: Place

  /** Every entry it names: an open section's entries, a closed one's closing. */
  def names: Vector[EntryId] = this match {
    case Open(_, _, entries) => entries
    case Closed(_, _, closing) => Vector(closing)
  }
}
