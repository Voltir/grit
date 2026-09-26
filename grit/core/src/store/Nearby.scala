package grit.core.store

import grit.core.id.{ConversationId, EntryId}
import grit.core.place.Place

/** What a window shows from another conversation's open period: `entries` of that
  * conversation, in its order, under the place they happened in.
  */
final case class Nearby(conversation: ConversationId, place: Place, entries: Vector[EntryId])
